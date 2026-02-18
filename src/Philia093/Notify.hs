module Philia093.Notify
  ( MonadNotify (..),
    WebhookNotifyT (..),
    runWebhookNotifyT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Text (Text)
import qualified Data.Text as T
import Philia093.Types

-- | Typeclass for notification operations
class (Monad m) => MonadNotify m where
  sendNotification :: Text -> m ()
  sendErrorNotification :: AppError -> m ()

-- | Webhook-based notification transformer
newtype WebhookNotifyT m a = WebhookNotifyT
  {unWebhookNotifyT :: ReaderT NotifyConfig m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

deriving newtype instance (MonadError AppError m) => MonadError AppError (WebhookNotifyT m)

runWebhookNotifyT :: NotifyConfig -> WebhookNotifyT m a -> m a
runWebhookNotifyT config = flip runReaderT config . unWebhookNotifyT

-- | Webhook implementation
-- TODO: Similar to other modules, logging mixed with business logic.
-- Consider extracting the logging into a separate concern:
--   instance (MonadIO m, MonadLog m) => MonadNotify (WebhookNotifyT m) where
--     sendNotification msg = do
--       config <- ask
--       logInfo $ "Sending to " <> webhookUrl config
--       -- actual HTTP call here
--
-- Also consider using `asks` instead of `ask >>= pattern match`:
--   sendNotification message = do
--     url <- asks webhookUrl
--     liftIO $ sendToWebhook url message
instance (MonadIO m) => MonadNotify (WebhookNotifyT m) where
  sendNotification message = do
    config <- WebhookNotifyT ask
    liftIO . putStrLn $
      "Sending webhook to "
        <> T.unpack (webhookUrl config)
        <> ": "
        <> T.unpack message

  -- Real implementation would use http-conduit here

  sendErrorNotification err = do
    let errorMsg = "⚠️ Error: " <> T.pack (show err)
    sendNotification errorMsg

-- | Lift through other transformers
instance (MonadNotify m) => MonadNotify (ReaderT r m) where
  sendNotification = lift . sendNotification
  sendErrorNotification = lift . sendErrorNotification
