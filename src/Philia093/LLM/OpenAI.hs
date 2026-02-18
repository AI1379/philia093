module Philia093.LLM.OpenAI
  ( OpenAIT,
    runOpenAIT,
  )
where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Philia093.LLM.LLMProvider
import Philia093.LLM.LLMTypes

-- | OpenAI transformer - the Haskell way!
-- Uses ReaderT for config and ExceptT for errors
newtype OpenAIT m a = OpenAIT
  {unOpenAIT :: ReaderT LLMConfig (ExceptT LLMError m) a}
  deriving (Functor, Applicative, Monad, MonadReader LLMConfig, MonadError LLMError, MonadIO)

-- | Run the OpenAI transformer
runOpenAIT :: LLMConfig -> OpenAIT m a -> m (Either LLMError a)
runOpenAIT config = runExceptT . flip runReaderT config . unOpenAIT

-- | OpenAI implementation of MonadLLM
instance (MonadIO m) => MonadLLM (OpenAIT m) where
  complete = performRequest

-- | Internal function to perform the HTTP request
-- Now uses MonadReader and MonadError!
-- TODO: Consider using more point-free style and applicative composition:
--   performRequest req = do
--     config <- ask
--     let endpoint = buildEndpoint config
--         headers = buildHeaders config
--     liftIO . doHttpRequest endpoint headers $ encode req
--       >>= parseResponse
-- Or use `ask >>=` with pure composition:
--   performRequest = ask <&> buildRequest >>> liftIO . performHttp >>= parseResponse
-- Also consider extracting the request building into a pure function:
--   buildHttpRequest :: LLMConfig -> LLMRequest -> Request
performRequest ::
  (MonadIO m, MonadReader LLMConfig m, MonadError LLMError m) =>
  LLMRequest ->
  m LLMResponse
performRequest req = do
  config <- ask -- Get config from Reader context

  -- Build HTTP request using pure functions
  let endpoint = buildEndpoint config
      headers = buildHeaders config
      requestBody = encode req

  -- Perform the HTTP call
  response <- liftIO $ doHttpRequest endpoint headers requestBody

  -- Parse response functionally
  parseResponse response

-- | Pure function to build endpoint URL
buildEndpoint :: LLMConfig -> String
buildEndpoint config = unpack (baseUrl config <> "/chat/completions")

-- | Pure function to build headers
buildHeaders :: LLMConfig -> RequestHeaders
buildHeaders config =
  [ (hAuthorization, encodeUtf8 $ "Bearer " <> apiKey config),
    (hContentType, encodeUtf8 "application/json")
  ]

-- | IO action to perform HTTP request
doHttpRequest :: String -> RequestHeaders -> ByteString -> IO (Response ByteString)
doHttpRequest endpoint headers body = do
  initialRequest <- parseRequest endpoint
  let request =
        initialRequest
          { method = "POST",
            requestHeaders = headers,
            requestBody = RequestBodyLBS body
          }
  manager <- newManager tlsManagerSettings
  httpLbs request manager

-- | Parse HTTP response - uses MonadError for error handling
-- TODO: This nested if-then-else style is imperative. Consider using
-- pattern matching with guards or `either` pattern:
--   parseResponse response = 
--     case statusCode (responseStatus response) of
--       200 -> decode (responseBody response)
--                & maybe (throwError parseError) pure
--       code -> decode (responseBody response)
--                 & maybe (throwError $ httpError code) throwError
--   
-- Or use `either` from `Control.Error` for clean error handling:
--   parseResponse = \response -> 
--     if success (statusCode $ responseStatus response)
--       then hoistEither . note parseError $ decode (responseBody response)
--       else hoistEither . note httpError $ decode (responseBody response)
--   
-- Or even more functional with `maybe`:
--   parseResponse response
--     | statusIsOk (responseStatus response) = 
--         maybe (throwError parseError) pure $ decode body
--     | otherwise = 
--         maybe (throwError httpError) throwError $ decode body
parseResponse ::
  (MonadError LLMError m) =>
  Response ByteString ->
  m LLMResponse
parseResponse response = do
  let statusCode' = statusCode (responseStatus response)
      body = responseBody response

  if statusCode' == 200
    then case decode body of
      Just llmResponse -> pure llmResponse
      Nothing ->
        throwError $
          LLMError
            { errorCode = 500,
              errorMessage = "Failed to parse response JSON"
            }
    else case decode body of
      Just llmError -> throwError llmError
      Nothing ->
        throwError $
          LLMError
            { errorCode = statusCode',
              errorMessage = "HTTP Error: " <> pack (show statusCode')
            }
