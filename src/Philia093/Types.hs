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
import Data.Text (Text)
import GHC.Generics
import Philia093.Email.EmailTypes
import Philia093.LLM.LLMTypes

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

instance ToJSON ProcessResult

instance FromJSON ProcessResult

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
