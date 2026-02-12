module Philia093.LLM.LLMProvider where

import Control.Concurrent.Async
import Philia093.LLM.LLMTypes

-- | Interface for LLM operations.
-- Implementation should be created with specific provider configuration (apiKey, baseUrl)
-- hidden via closures, so the caller doesn't need to pass them every time.
data LLMProviderHandle = LLMProviderHandle
  { -- | Synchronous completion request
    complete :: LLMRequest -> IO (Either LLMError LLMResponse),
    -- | Asynchronous completion request
    completeAsync :: LLMRequest -> IO (Async (Either LLMError LLMResponse))
  }