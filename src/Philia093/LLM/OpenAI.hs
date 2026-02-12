module Philia093.LLM.OpenAI (mkOpenAIProvider) where

import Control.Concurrent.Async (async)
import Philia093.LLM.LLMProvider
import Philia093.LLM.LLMTypes

-- | Create a Handle for OpenAI.
-- Notice how LLMConfig is "baked into" the functions.
mkOpenAIProvider :: LLMConfig -> LLMProviderHandle
mkOpenAIProvider config =
  LLMProviderHandle
    { complete = performRequest config,
      completeAsync = async . performRequest config
    }

-- | Internal function to perform the HTTP request
performRequest :: LLMConfig -> LLMRequest -> IO (Either LLMError LLMResponse)
performRequest _config _req = do
  -- Here you would use http-conduit to call OpenAI using config's apiKey and baseUrl
  -- For now, returning a placeholder
  return $ Left (LLMError 501 "Not Implemented Yet")
