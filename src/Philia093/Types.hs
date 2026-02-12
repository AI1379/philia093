module Philia093.Types where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics

-- | Represents a simple email structure
data Email = Email
  { emailId :: Text,
    emailFrom :: Text,
    emailSubject :: Text,
    emailBody :: Text,
    emailAttach :: [Attachment]
  }
  deriving (Show, Generic)

data Attachment = Attachment
  { attachName :: Text,
    attachType :: Text,
    attachContent :: ByteString
  }
  deriving (Show, Generic)

-- | Result of processing an email
data ProcessResult = ProcessResult
  { prHandled :: Bool,
    prSummary :: Text,
    prShouldNotify :: Bool
  }
  deriving (Show, Generic)

instance ToJSON Email

instance FromJSON Email

instance ToJSON ProcessResult

instance FromJSON ProcessResult

instance ToJSON Attachment where
  toJSON Attachment {..} =
    object
      [ "attachName" .= attachName,
        "attachType" .= attachType,
        "attachContent" .= TE.decodeUtf8 (B64.encode attachContent)
      ]

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \v -> do
    name <- v .: "attachName"
    typ <- v .: "attachType"
    contentB64 <- v .: "attachContent"
    let content = case B64.decode (TE.encodeUtf8 contentB64) of
          Left err -> error $ "Base64 decode error: " ++ err
          Right val -> val
    return $ Attachment name typ content

-- | App configuration
data Config = Config
  { imapHost :: Text,
    imapUser :: Text,
    imapPass :: Text,
    webhookUrl :: Text,
    llmApiKey :: Text,
    llmApiBaseUrl :: Text,
    llmModel :: Text,
    smtpHost :: Text
  }
  deriving (Show, Generic)

instance ToJSON Config

instance FromJSON Config
