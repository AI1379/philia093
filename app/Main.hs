module Main where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Yaml (decodeFileThrow)
import Philia093.App
import Philia093.Email ()
import Philia093.Notify ()
import Philia093.Processor ()
import Philia093.Types
import System.Exit (exitFailure)

-- | Simplified application monad stack using the ReaderT pattern
-- Instead of deep transformer nesting, we compose just Reader + Error + IO
-- All services access configuration via a single unified BotEnv
type BotM = ReaderT BotEnv (ExceptT AppError IO)

-- | Run the bot with a flat 2-layer transformer stack
-- Much cleaner than the previous 7-layer deep stack!
runBotM :: BotEnv -> BotM a -> IO (Either AppError a)
runBotM env action = runExceptT (runReaderT action env)

main :: IO ()
main = do
  putStrLn "🚀 Welcome to Philia093 Bot!"
  putStrLn "📂 Loading configuration from: config.yaml"

  -- Load configuration
  config <- decodeFileThrow "config.yaml" :: IO AppConfig
  putStrLn $ "✅ Configuration loaded: " <> show config

  -- Create bot environment from config
  let env = appConfigToBotEnv config

  -- Run the bot with the simplified monad stack
  putStrLn "\n🤖 Starting bot with ReaderT Pattern..."
  result <- runBotM env runBot

  -- Handle the result
  case result of
    Left err -> do
      putStrLn $ "❌ Fatal error: " <> show err
      exitFailure
    Right _ ->
      putStrLn "✨ Bot completed successfully!"
