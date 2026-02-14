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
runBotM :: AppEnv -> BotM a -> IO (Either AppError a)
runBotM env action =
  runExceptT $
    flip runReaderT env $
      runWebhookNotifyT (notifyConfig . appConfig $ env) $
        runSimpleProcessorT $
          runMockEmailT (emailConfig . appConfig $ env) $
            action

main :: IO ()
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
