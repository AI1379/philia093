module Philia093.LLM.LLMTypes where

import Control.Exception (Exception)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A message in the conversation with the LLM
data LLMMessage = LLMMessage
  { role :: Text,
    content :: Text
  }
  deriving (Show, Generic)

-- | Request to the LLM API
data LLMRequest = LLMRequest
  { model :: Text,
    messages :: [LLMMessage],
    temperature :: Maybe Double,
    maxTokens :: Maybe Int
  }
  deriving (Show, Generic)

-- | A single choice returned by the LLM
data LLMChoice = LLMChoice
  { index :: Int,
    message :: LLMMessage,
    finishReason :: Maybe Text
  }
  deriving (Show, Generic)

-- | Response from the LLM API
data LLMResponse = LLMResponse
  { id :: Text,
    model :: Text,
    choices :: [LLMChoice]
  }
  deriving (Show, Generic)

-- | Error types for LLM operations
data LLMError = LLMError
  { errorCode :: Int,
    errorMessage :: Text
  }
  deriving (Show, Generic)

instance Exception LLMError

-- | Configuration for an LLM provider
data LLMConfig = LLMConfig
  { apiKey :: Text,
    baseUrl :: Text,
    defaultModel :: Text
  }
  deriving (Show, Generic)

-- JSON instances
instance FromJSON LLMConfig

instance ToJSON LLMConfig

instance FromJSON LLMMessage

instance ToJSON LLMMessage

instance FromJSON LLMRequest where
  parseJSON = withObject "LLMRequest" $ \v ->
    LLMRequest
      <$> v .: "model"
      <*> v .: "messages"
      <*> v .:? "temperature"
      <*> v .:? "max_tokens"

instance ToJSON LLMRequest where
  toJSON LLMRequest {..} =
    object $
      [ "model" .= model,
        "messages" .= messages
      ]
        ++ maybe [] (\t -> ["temperature" .= t]) temperature
        ++ maybe [] (\m -> ["max_tokens" .= m]) maxTokens

instance FromJSON LLMChoice where
  parseJSON = withObject "LLMChoice" $ \v ->
    LLMChoice
      <$> v .: "index"
      <*> v .: "message"
      <*> v .:? "finish_reason"

instance ToJSON LLMChoice where
  toJSON LLMChoice {..} =
    object $
      [ "index" .= index,
        "message" .= message
      ]
        ++ maybe [] (\r -> ["finish_reason" .= r]) finishReason

instance FromJSON LLMResponse

instance ToJSON LLMResponse

instance FromJSON LLMError where
  parseJSON = withObject "LLMError" $ \v ->
    LLMError
      <$> v .: "code"
      <*> v .: "message"

instance ToJSON LLMError where
  toJSON LLMError {..} =
    object
      [ "code" .= errorCode,
        "message" .= errorMessage
      ]

-- | Smart constructors for better API
mkSystemMessage :: Text -> LLMMessage
mkSystemMessage = LLMMessage "system"

mkUserMessage :: Text -> LLMMessage
mkUserMessage = LLMMessage "user"

mkAssistantMessage :: Text -> LLMMessage
mkAssistantMessage = LLMMessage "assistant"

-- | Build a simple LLM request
mkLLMRequest :: Text -> Text -> LLMRequest
mkLLMRequest model prompt =
  LLMRequest
    { model = model,
      messages = [mkUserMessage prompt],
      temperature = Nothing,
      maxTokens = Nothing
    }
