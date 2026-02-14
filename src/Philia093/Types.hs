module Philia093.Types
  ( Email (..),
    EmailId (..),
    Attachment (..),
    ProcessResult (..),
    AppError (..),
    EmailConfig (..),
    NotifyConfig (..),
    AppConfig (..),
  )
where

import Control.Exception (Exception)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics
import Philia093.LLM.LLMTypes

-- | Newtype wrapper for email IDs to avoid primitive obsession
newtype EmailId = EmailId {unEmailId :: Text}
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, ToJSON, FromJSON)

-- | Represents an email structure with better naming
data Email = Email
  { emailId :: EmailId,
    sender :: Text,
    subject :: Text,
    body :: Text,
    attachments :: [Attachment]
  }
  deriving (Show, Generic)

-- | Email attachment with better naming
data Attachment = Attachment
  { fileName :: Text,
    mimeType :: Text,
    content :: ByteString
  }
  deriving (Show, Generic)

-- | Result of processing an email - no prefixes!
data ProcessResult = ProcessResult
  { handled :: Bool,
    summary :: Text,
    shouldNotify :: Bool
  }
  deriving (Show, Generic)

-- | Application errors - proper sum type for different error cases
data AppError
  = EmailFetchError Text
  | EmailProcessError Text
  | NotificationError Text
  | LLMError' LLMError
  | ConfigError Text
  deriving (Show, Generic)

instance Exception AppError

-- | Email configuration
data EmailConfig = EmailConfig
  { imapHost :: Text,
    imapPort :: Int,
    imapUser :: Text,
    imapPassword :: Text,
    smtpHost :: Text,
    smtpPort :: Int
  }
  deriving (Show, Generic)

-- | Notification configuration
newtype NotifyConfig = NotifyConfig
  { webhookUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Complete application configuration
data AppConfig = AppConfig
  { emailConfig :: EmailConfig,
    notifyConfig :: NotifyConfig,
    llmConfig :: LLMConfig
  }
  deriving (Show, Generic)

-- JSON instances with better field mapping
instance ToJSON Email where
  toJSON Email {..} =
    object
      [ "id" .= emailId,
        "sender" .= sender,
        "subject" .= subject,
        "body" .= body,
        "attachments" .= attachments
      ]

instance FromJSON Email where
  parseJSON = withObject "Email" $ \v ->
    Email
      <$> v .: "id"
      <*> v .: "sender"
      <*> v .: "subject"
      <*> v .: "body"
      <*> v .:? "attachments" .!= []

instance ToJSON ProcessResult

instance FromJSON ProcessResult

instance ToJSON Attachment where
  toJSON Attachment {..} =
    object
      [ "fileName" .= fileName,
        "mimeType" .= mimeType,
        "content" .= TE.decodeUtf8 (B64.encode content)
      ]

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \v -> do
    name <- v .: "fileName"
    mime <- v .: "mimeType"
    contentB64 <- v .: "content"
    case B64.decode (TE.encodeUtf8 contentB64) of
      Left err -> fail $ "Base64 decode error: " <> err
      Right val -> pure $ Attachment name mime val

instance ToJSON EmailConfig

instance FromJSON EmailConfig

instance ToJSON AppConfig where
  toJSON AppConfig {..} =
    object
      [ "email" .= emailConfig,
        "notify" .= notifyConfig,
        "llm" .= llmConfig
      ]

instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \v ->
    AppConfig
      <$> v .: "email"
      <*> v .: "notify"
      <*> v .: "llm"
