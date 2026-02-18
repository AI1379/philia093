module Main where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Yaml (decodeFileThrow)
import Philia093.App
import Philia093.Email
import Philia093.Notify
import Philia093.Processor
import Philia093.Types
import System.Exit (exitFailure)

-- | Complete application monad stack - composing all transformers!
-- This is the Haskell way: compose capabilities through transformers
type BotM = MockEmailT (SimpleProcessorT (WebhookNotifyT (ReaderT AppEnv (ExceptT AppError IO))))

-- | Run the complete bot with all transformers
-- TODO: Deep transformer stacks like this are a code smell in modern Haskell.
-- Consider using "ReaderT Pattern" with explicit runtime:
--   type BotM = ReaderT BotEnv (ExceptT AppError IO)
--   runBotM :: BotEnv -> BotM a -> IO (Either AppError a)
-- Then implement capabilities as fields in BotEnv:
--   data BotEnv = BotEnv
--     { emailService :: EmailService
--     , notifyService :: NotifyService  
--     , processorService :: ProcessorService
--     , llmService :: LLMService
--     }
-- This avoids "transformer soup" and gives better type inference.
-- See: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
runBotM :: AppEnv -> BotM a -> IO (Either AppError a)
runBotM env action =
  runExceptT $
    flip runReaderT env $
      runWebhookNotifyT (notifyConfig . appConfig $ env) $
        runSimpleProcessorT $
          runMockEmailT (emailConfig . appConfig $ env) $
            action

main :: IO ()
-- TODO: This imperative-style main could be more declarative:
--   main :: IO ()
--   main = 
--     loadConfig "config.yaml"
--       >>= runBotWithConfig
--       >>= either exitWithError pure
--   
--   where
--     loadConfig path = putStrLn "📂 Loading..." *> decodeFileThrow path
--     runBotWithConfig cfg = runBotM (AppEnv cfg) runBot
--     exitWithError err = putStrLn ("❌ " <> show err) *> exitFailure
--   
-- Or use `bracket` for resource-safe config loading:
--   main = bracket loadConfig cleanup runBot
main = do
  putStrLn "🚀 Welcome to Philia093 Bot!"
  putStrLn "📂 Loading configuration from: config.yaml"

  -- Load configuration
  config <- decodeFileThrow "config.yaml" :: IO AppConfig
  putStrLn $ "✅ Configuration loaded: " <> show config

  -- Create application environment
  let env = AppEnv {appConfig = config}

  -- Run the bot with the complete monad stack
  putStrLn "\n🤖 Starting bot with MTL-style architecture..."
  result <- runBotM env runBot

  -- Handle the result
  case result of
    Left err -> do
      putStrLn $ "❌ Fatal error: " <> show err
      exitFailure
    Right _ ->
      putStrLn "✨ Bot completed successfully!"
