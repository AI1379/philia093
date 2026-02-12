module Philia093.Notify where

import Data.Text (Text)

-- | Interface for Notification operations
newtype NotifyHandle = NotifyHandle {notifyWebhook :: Text -> IO ()}

-- | Webhook implementation
mkWebhookNotify :: Text -> NotifyHandle
mkWebhookNotify _url =
  NotifyHandle
    { notifyWebhook = \msg -> putStrLn ("Sending Webhook: " <> show msg)
    }
