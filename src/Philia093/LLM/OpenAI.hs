{-# LANGUAGE OverloadedStrings #-}

module Philia093.LLM.OpenAI (makeOpenAIProvider) where

import Control.Concurrent.Async (async)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Conduit
import Network.HTTP.Types (hAuthorization, hContentType, statusCode, statusMessage)
import Philia093.LLM.LLMProvider
import Philia093.LLM.LLMTypes

-- | Create a Handle for OpenAI.
-- Notice how LLMConfig is "baked into" the functions.
makeOpenAIProvider :: LLMConfig -> LLMProviderHandle
makeOpenAIProvider config =
  LLMProviderHandle
    { complete = performRequest config,
      completeAsync = async . performRequest config
    }

-- | Internal function to perform the HTTP request
performRequest :: LLMConfig -> LLMRequest -> IO (Either LLMError LLMResponse)
performRequest config req = do
  -- Construct the endpoint URL
  let endpoint = unpack (baseUrl config <> "/chat/completions")

  -- Construct request headers with authorization
  let headers =
        [ (hAuthorization, encodeUtf8 ("Bearer " <> apiKey config)),
          (hContentType, encodeUtf8 "application/json")
        ]

  -- Encode the request body as JSON
  let requestBody = encode req

  -- Parse the URL and create the base request
  initialRequest <- parseRequest endpoint

  -- Build the final request
  let request =
        initialRequest
          { method = "POST",
            requestHeaders = headers,
            requestBody = RequestBodyLBS requestBody
          }

  -- Create HTTP manager and send the request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager

  -- Extract status code and response body
  let statusCode' = statusCode (responseStatus response)
      body = responseBody response

  -- Handle response based on status code
  if statusCode' == 200
    then case decode body :: Maybe LLMResponse of
      Just llmResponse -> return $ Right llmResponse
      Nothing -> return $ Left (LLMError 500 "Failed to parse response JSON")
    else case decode body :: Maybe LLMError of
      Just llmError -> return $ Left llmError
      Nothing ->
        return $
          Left
            ( LLMError
                statusCode'
                (pack $ "HTTP Error: " <> show statusCode')
            )
