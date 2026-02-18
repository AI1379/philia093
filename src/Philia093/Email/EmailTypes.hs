module Philia093.Email.EmailTypes where

import Control.Exception
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.Word (Word64)

data Address = Address
  { name :: Maybe Text,
    email :: Text
  }
  deriving (Show, Generic)

-- | Newtype wrapper for email IDs to avoid primitive obsession
newtype EmailId = EmailId {unEmailId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

-- | Represents an email structure with better naming and compatibility
data Email = Email
  { emailId :: EmailId,
    uid :: Maybe Word64,
    from :: [Address],
    to :: [Address],
    cc :: [Address],
    bcc :: [Address],
    subject :: Text,
    bodyText :: Maybe Text,
    bodyHtml :: Maybe Text,
    date :: Maybe UTCTime,
    attachments :: [Attachment],
    headers :: [(Text, Text)]
  }
  deriving (Show, Generic)

newtype Mailbox = Mailbox {unMailbox :: Text}
  deriving (Show, Generic)

-- | Email attachment with better naming and MIME compatibility
data Attachment = Attachment
  { fileName :: Text,
    contentType :: Text,
    contentId :: Maybe Text,
    content :: ByteString
  }
  deriving (Show, Generic)

-- | Security settings for connections
data Security
  = None
  | SSL
  | STARTTLS
  deriving (Show, Generic, Eq)

-- | Email configuration
data IMAPConfig = IMAPConfig
  { accountName :: Text,
    host :: Text,
    port :: Int,
    user :: Text,
    password :: Text,
    useSecurity :: Security
  }
  deriving (Show, Generic)

data SMTPConfig = SMTPConfig
  { accountName :: Text,
    host :: Text,
    port :: Int,
    user :: Text,
    password :: Text,
    useSecurity :: Security
  }
  deriving (Show, Generic)

data EmailConfig = EmailConfig
  { imap :: IMAPConfig,
    smtp :: SMTPConfig
  }
  deriving (Show, Generic)

-- JSON instances

instance ToJSON Address

instance FromJSON Address

instance ToJSON Security

instance FromJSON Security

instance ToJSON Email where
  toJSON Email {..} =
    object
      [ "id" .= emailId,
        "uid" .= uid,
        "from" .= from,
        "to" .= to,
        "cc" .= cc,
        "bcc" .= bcc,
        "subject" .= subject,
        "bodyText" .= bodyText,
        "bodyHtml" .= bodyHtml,
        "date" .= date,
        "attachments" .= attachments,
        "headers" .= headers
      ]

instance FromJSON Email where
  parseJSON = withObject "Email" $ \v ->
    Email
      <$> v .: "id"
      <*> v .:? "uid"
      <*> v .: "from"
      <*> v .: "to"
      <*> v .:? "cc" .!= []
      <*> v .:? "bcc" .!= []
      <*> v .: "subject"
      <*> v .:? "bodyText"
      <*> v .:? "bodyHtml"
      <*> v .:? "date"
      <*> v .:? "attachments" .!= []
      <*> v .:? "headers" .!= []

instance ToJSON Attachment where
  toJSON Attachment {..} =
    object
      [ "fileName" .= fileName,
        "contentType" .= contentType,
        "contentId" .= contentId,
        "content" .= TE.decodeUtf8 (B64.encode content)
      ]

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \v -> do
    name <- v .: "fileName"
    mime <- v .: "contentType"
    cid <- v .:? "contentId"
    contentB64 <- v .: "content"
    case B64.decode (TE.encodeUtf8 contentB64) of
      Left err -> fail $ "Base64 decode error: " <> err
      Right val -> pure $ Attachment name mime cid val

instance ToJSON IMAPConfig

instance FromJSON IMAPConfig

instance ToJSON SMTPConfig

instance FromJSON SMTPConfig

instance ToJSON EmailConfig

instance FromJSON EmailConfig

-- | Shared error types for both IMAP and SMTP protocols
-- | These errors can occur in any email protocol operation
data ProtocolError
  = -- | Connection-related errors
    ConnError {host :: Text, port :: Int, reason :: Text}
  | -- | Authentication/login failures
    AuthFailure {account :: Text, reason :: Text}
  | -- | Timeout errors
    OpTimeout {operation :: Text, seconds :: Int}
  | -- | Generic parsing/encoding errors
    ParseErr {reason :: Text}
  | -- | Server protocol errors
    ProtoViolation {reason :: Text, details :: Maybe Text}
  deriving (Show, Generic, Eq)

-- | Helper smart constructors for protocol errors
connError :: Text -> Int -> String -> ProtocolError
connError h p r = ConnError h p (pack r)

authFailure :: Text -> String -> ProtocolError
authFailure a r = AuthFailure a (pack r)

opTimeout :: String -> Int -> ProtocolError
opTimeout op sec = OpTimeout (pack op) sec

parseErr :: String -> ProtocolError
parseErr r = ParseErr (pack r)

protoViolation :: String -> Maybe String -> ProtocolError
protoViolation r d = ProtoViolation (pack r) (pack <$> d)

instance ToJSON ProtocolError where
  toJSON err = object $ case err of
    ConnError h p r ->
      [ "type" .= ("ConnError" :: Text),
        "host" .= h,
        "port" .= p,
        "reason" .= r
      ]
    AuthFailure a r ->
      [ "type" .= ("AuthFailure" :: Text),
        "account" .= a,
        "reason" .= r
      ]
    OpTimeout op sec ->
      [ "type" .= ("OpTimeout" :: Text),
        "operation" .= op,
        "seconds" .= sec
      ]
    ParseErr r ->
      [ "type" .= ("ParseErr" :: Text),
        "reason" .= r
      ]
    ProtoViolation r d ->
      [ "type" .= ("ProtoViolation" :: Text),
        "reason" .= r,
        "details" .= d
      ]

instance FromJSON ProtocolError where
  parseJSON = withObject "ProtocolError" $ \v -> do
    errType <- v .: "type"
    case (errType :: Text) of
      "ConnError" ->
        ConnError
          <$> v .: "host"
          <*> v .: "port"
          <*> v .: "reason"
      "AuthFailure" ->
        AuthFailure
          <$> v .: "account"
          <*> v .: "reason"
      "OpTimeout" ->
        OpTimeout
          <$> v .: "operation"
          <*> v .: "seconds"
      "ParseErr" ->
        ParseErr
          <$> v .: "reason"
      "ProtoViolation" ->
        ProtoViolation
          <$> v .: "reason"
          <*> v .:? "details"
      _ -> fail $ "Unknown ProtocolError type: " <> unpack errType

instance Exception ProtocolError

-- | IMAP-specific error types that extend protocol errors
data IMAPError
  = -- | Protocol-level errors shared with SMTP
    IMAPProtoError ProtocolError
  | -- | Mailbox operation errors (IMAP-specific)
    MailboxError {mailbox :: Maybe Text, operation :: Text, reason :: Text}
  | -- | Message/Email parsing or fetching errors (IMAP-specific)
    MessageError {messageId :: Maybe Text, reason :: Text}
  | -- | Generic IMAP error with context
    IMAPErrorGeneric {reason :: Text, context :: Maybe Text}
  deriving (Show, Generic, Eq)

-- | Helper to wrap protocol errors as IMAP errors
wrapProtocolError :: ProtocolError -> IMAPError
wrapProtocolError = IMAPProtoError

mailboxError :: Maybe Text -> String -> String -> IMAPError
mailboxError m op r = MailboxError m (pack op) (pack r)

messageError :: Maybe Text -> String -> IMAPError
messageError mid r = MessageError mid (pack r)

imapErrorGeneric :: String -> Maybe String -> IMAPError
imapErrorGeneric r ctx = IMAPErrorGeneric (pack r) (pack <$> ctx)

instance ToJSON IMAPError where
  toJSON err = object $ case err of
    IMAPProtoError protoErr ->
      [ "type" .= ("IMAPProtoError" :: Text),
        "protocolError" .= toJSON protoErr
      ]
    MailboxError m op r ->
      [ "type" .= ("MailboxError" :: Text),
        "mailbox" .= m,
        "operation" .= op,
        "reason" .= r
      ]
    MessageError mid r ->
      [ "type" .= ("MessageError" :: Text),
        "messageId" .= mid,
        "reason" .= r
      ]
    IMAPErrorGeneric r ctx ->
      [ "type" .= ("IMAPErrorGeneric" :: Text),
        "reason" .= r,
        "context" .= ctx
      ]

instance FromJSON IMAPError where
  parseJSON = withObject "IMAPError" $ \v -> do
    errType <- v .: "type"
    case (errType :: Text) of
      "IMAPProtoError" ->
        IMAPProtoError
          <$> v .: "protocolError"
      "MailboxError" ->
        MailboxError
          <$> v .:? "mailbox"
          <*> v .: "operation"
          <*> v .: "reason"
      "MessageError" ->
        MessageError
          <$> v .:? "messageId"
          <*> v .: "reason"
      "IMAPErrorGeneric" ->
        IMAPErrorGeneric
          <$> v .: "reason"
          <*> v .:? "context"
      _ -> fail $ "Unknown IMAPError type: " <> unpack errType

instance Exception IMAPError

-- | SMTP-specific error types that extend protocol errors  
data SMTPError
  = -- | Protocol-level errors shared with IMAP
    SMTPProtoError ProtocolError
  | -- | Recipient/delivery errors (SMTP-specific)
    RecipientError {recipient :: Text, reason :: Text}
  | -- | Message composition errors (SMTP-specific)
    MessageCompositionError {reason :: Text}
  | -- | Generic SMTP error with context
    SMTPErrorGeneric {reason :: Text, context :: Maybe Text}
  deriving (Show, Generic, Eq)

-- | Helper to wrap protocol errors as SMTP errors
wrapSMTPProtocolError :: ProtocolError -> SMTPError
wrapSMTPProtocolError = SMTPProtoError

recipientError :: Text -> String -> SMTPError
recipientError recp r = RecipientError recp (pack r)

messageCompositionError :: String -> SMTPError
messageCompositionError r = MessageCompositionError (pack r)

smtpErrorGeneric :: String -> Maybe String -> SMTPError
smtpErrorGeneric r ctx = SMTPErrorGeneric (pack r) (pack <$> ctx)

instance ToJSON SMTPError where
  toJSON err = object $ case err of
    SMTPProtoError protoErr ->
      [ "type" .= ("SMTPProtoError" :: Text),
        "protocolError" .= toJSON protoErr
      ]
    RecipientError recp r ->
      [ "type" .= ("RecipientError" :: Text),
        "recipient" .= recp,
        "reason" .= r
      ]
    MessageCompositionError r ->
      [ "type" .= ("MessageCompositionError" :: Text),
        "reason" .= r
      ]
    SMTPErrorGeneric r ctx ->
      [ "type" .= ("SMTPErrorGeneric" :: Text),
        "reason" .= r,
        "context" .= ctx
      ]

instance FromJSON SMTPError where
  parseJSON = withObject "SMTPError" $ \v -> do
    errType <- v .: "type"
    case (errType :: Text) of
      "SMTPProtoError" ->
        SMTPProtoError
          <$> v .: "protocolError"
      "RecipientError" ->
        RecipientError
          <$> v .: "recipient"
          <*> v .: "reason"
      "MessageCompositionError" ->
        MessageCompositionError
          <$> v .: "reason"
      "SMTPErrorGeneric" ->
        SMTPErrorGeneric
          <$> v .: "reason"
          <*> v .:? "context"
      _ -> fail $ "Unknown SMTPError type: " <> unpack errType

instance Exception SMTPError

-- | Unified error type for email operations (both IMAP and SMTP)
data EmailError
  = IMAPEmailError IMAPError
  | SMTPEmailError SMTPError
  deriving (Show, Generic)

instance ToJSON EmailError where
  toJSON err = case err of
    IMAPEmailError imapErr ->
      object
        [ "type" .= ("IMAPEmailError" :: Text),
          "error" .= toJSON imapErr
        ]
    SMTPEmailError smtpErr ->
      object
        [ "type" .= ("SMTPEmailError" :: Text),
          "error" .= toJSON smtpErr
        ]

instance FromJSON EmailError where
  parseJSON = withObject "EmailError" $ \v -> do
    errType <- v .: "type"
    case (errType :: Text) of
      "IMAPEmailError" ->
        IMAPEmailError
          <$> v .: "error"
      "SMTPEmailError" ->
        SMTPEmailError
          <$> v .: "error"
      _ -> fail $ "Unknown EmailError type: " <> unpack errType

instance Exception EmailError
