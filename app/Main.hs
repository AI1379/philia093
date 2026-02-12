module Main where

import Data.Text (pack)
import Data.Yaml (decodeFileThrow)
import Philia093.App
import Philia093.Email
import Philia093.Notify
import Philia093.Processor
import Philia093.Types

main :: IO ()
main = do
  putStrLn "Welcome to Philia093 Bot!"
  let configPath = "config.yaml"
  putStrLn $ "Loading configuration from: " ++ configPath

  _config <- decodeFileThrow configPath :: IO Config

  let _config_str = show _config
  putStrLn $ "Configuration loaded: " ++ _config_str

  -- Wiring the modular components (Handles)
  let handlers =
        AppHandlers
          { emailH = mkMockEmailHandle, -- Easy to swap with real IMAP
            processorH = mkSimpleProcessor, -- Easy to swap with PDF/LLM logic
            notifyH = mkWebhookNotify (webhookUrl _config)
          }

  -- Run the bot
  runBot handlers
