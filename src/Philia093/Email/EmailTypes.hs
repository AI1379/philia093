module Philia093.Email.EmailTypes where

import Control.Exception
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import GHC.Generics (Generic)

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

-- | Exception types for better error handling
newtype IMAPError = IMAPError Text
  deriving (Show, Generic)

newtype SMTPError = SMTPError Text
  deriving (Show, Generic)

data EmailError
  = EmailFetchError IMAPError
  | EmailSendError SMTPError
  deriving (Show, Generic)

instance Exception IMAPError

instance Exception SMTPError

instance Exception EmailError
