-- | Email module - handles email protocol operations (IMAP/SMTP)
-- 
-- 长期计划：将此模块分离为：
-- - Philia093.Protocol.Email :: IMAP/SMTP 协议实现（Low-level）
-- - Philia093.InfoSource.Email :: Email 作为 InfoSource 实现（Business logic）
-- 
-- 这样可以更清晰地分离关注点：
-- - Protocol 处理邮件协议细节
-- - InfoSource 实现从邮件获取信息的业务逻辑
module Philia093.Email
  ( MonadEmail (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Philia093.Types

-- ============================================================================
-- | Typeclass for email operations
-- ============================================================================

-- | TypeClass for email protocol operations (current: IMAP operations)
-- This will eventually be split into:
-- - Protocol operations: fetchRawEmails, sendRawEmail, markAsRead (etc.)
-- - InfoSource operations: fetchArticles (higher-level business logic)
class (Monad m) => MonadEmail m where
  -- | Fetch emails from IMAP server (mock: returns no emails)
  fetchEmails :: m [Email]

  -- | Send an email via SMTP
  sendEmail :: Email -> m ()

  -- | Mark an email as read in IMAP
  markAsRead :: EmailId -> m ()

-- ============================================================================
-- | Instance for the main BotM monad stack
-- ============================================================================

instance (MonadIO m, MonadError AppError m) => MonadEmail (ReaderT BotEnv m) where
  fetchEmails = do
    _env <- ask
    -- Configuration is available via _env if needed in the future
    liftIO $ putStrLn "Mock: Fetching emails from IMAP"
    pure []

  sendEmail email =
    liftIO . putStrLn $ "Mock: Sending email via SMTP with subject: " <> show (subject email)

  markAsRead emailId =
    liftIO . putStrLn $ "Mock: Marking email as read in IMAP: " <> show emailId

-- ============================================================================
-- | Real implementations (TODO)
-- ============================================================================

-- | Real IMAP implementation would look like:
-- instance (MonadIO m, MonadError AppError m) => MonadEmail (ReaderT BotEnv m) where
--   fetchEmails = do
--     env <- ask
--     let config = botEmailConfig env
--     let IMAPConfig {..} = imap config
--     liftIO $ do
--       conn <- connectIMAP host port user password useSecurity
--       emails <- fetchUnread conn "INBOX"
--       closeConnection conn
--       pure emails
--
--   sendEmail email = do
--     env <- ask
--     let config = botEmailConfig env
--     let SMTPConfig {..} = smtp config
--     liftIO $ do
--       conn <- connectSMTP host port user password useSecurity
--       sendMailWithAuth conn (toSMTPMail email)
--       closeConnection conn
--
--   markAsRead emailId = do
--     -- 实现标记为已读的逻辑
--     pure ()

-- ============================================================================
-- | Architecture Notes for Email Module Refactor
-- ============================================================================

-- | TODO: 在完成所有其他 InfoSource 实现后，重构此模块
-- Current splitting plan:
--
-- 1. Philia093.Protocol.Email.IMAP (new)
--    - connectIMAP, fetchEmails, markAsRead
--    - Handle IMAP protocol details
--
-- 2. Philia093.Protocol.Email.SMTP (new)
--    - connectSMTP, sendEmail
--    - Handle SMTP protocol details
--
-- 3. Philia093.InfoSource.Email (new - for info retrieval)
--    - Implement MonadInfoSource
--    - Fetch emails as Article
--    - Extract metadata and content
--
-- 4. Philia093.Notify.Email (new - for notifications)
--    - Implement MonadNotify
--    - Send notifications via Email
--
--     let IMAPConfig {..} = imap (botEmailConfig env)
--     liftIO $ connectIMAP host port user password >>= fetchUnread
--   ...
