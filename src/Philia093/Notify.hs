-- | Notify module - improved structure with support for multiple notification types
-- This refactors the notification system to be more modular and extensible
module Philia093.Notify
  ( MonadNotify (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Text (Text)
import qualified Data.Text as T
import Philia093.Types

-- ============================================================================
-- | Typeclass for notification operations
-- ============================================================================

class (Monad m) => MonadNotify m where
  -- | Send a notification message
  sendNotification :: Text -> m ()

  -- | Send an error notification
  sendErrorNotification :: AppError -> m ()

-- ============================================================================
-- | Instance for the main BotM monad stack
-- ============================================================================

instance (MonadIO m, MonadError AppError m) => MonadNotify (ReaderT BotEnv m) where
  sendNotification message = do
    env <- ask
    let config = botNotifyConfig env
    liftIO . putStrLn $
      "Sending webhook to "
        <> T.unpack (webhookUrl config)
        <> ": "
        <> T.unpack message
    -- Real implementation would use http-conduit here to actually send the webhook

  sendErrorNotification err = do
    let errorMsg = "⚠️ Error: " <> T.pack (show err)
    sendNotification errorMsg

-- ============================================================================
-- | Future extensions for multiple notification methods
-- ============================================================================

-- | 架构说明：如何扩展支持多种通知方式
-- 
-- 目前 Notify 模块只支持 Webhook 通知。为了支持多种通知方式（邮件、Matrix、Telegram等），
-- 应该按照以下方式重组：
--
-- 1. 创建 Notify/Types.hs，定义所有通知配置：
--    data NotifyConfig = WebhookConfig Text | EmailConfig {...} | MatrixConfig {...}
--
-- 2. 创建具体实现模块：
--    - Philia093.Notify.Webhook  :: 现有的 Webhook 实现
--    - Philia093.Notify.Email    :: Email 通知实现
--    - Philia093.Notify.Matrix   :: Matrix 通知实现
--    - Philia093.Notify.Telegram :: Telegram 机器人通知
--
-- 3. 使用 DynNotifier 动态分发：
--    data DynNotifier m = forall n. MonadNotify n => DynNotifier (n ())
--    
--    handleNotification :: DynNotifier m -> Text -> m ()
--
-- 4. 在 AppEnv 中存储多通知器：
--    data AppEnv = AppEnv {
--      notifiers :: Map TargetId DynNotifier
--    }
--
-- 5. 更新流程以支持多目标发送：
--    processArticle :: Article -> AppM [ProcessResult]
--    notifyAll :: ProcessResult -> AppM ()  -- 发送给所有启用的通知器
