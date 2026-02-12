{-# LANGUAGE DuplicateRecordFields #-}

module Philia093.LLM.LLMTypes where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data LLMMessage = LLMMessage
  { role :: Text,
    content :: Text
  }
  deriving (Show, Generic)

data LLMRequest = LLMRequest
  { model :: Text,
    messages :: [LLMMessage]
  }
  deriving (Show, Generic)

data LLMChoice = LLMChoice
  { index :: Int,
    message :: LLMMessage
  }
  deriving (Show, Generic)

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

instance FromJSON LLMMessage

instance FromJSON LLMRequest

instance FromJSON LLMChoice

instance FromJSON LLMResponse

instance FromJSON LLMError

-- | Configuration specific to an LLM provider
data LLMConfig = LLMConfig
  { apiKey :: Text,
    baseUrl :: Text,
    defaultModel :: Text
  }
  deriving (Show, Generic)

instance ToJSON LLMMessage

instance ToJSON LLMRequest

instance ToJSON LLMChoice

instance ToJSON LLMResponse

instance ToJSON LLMError
