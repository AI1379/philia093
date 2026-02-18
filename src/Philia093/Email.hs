module Philia093.Email
  ( MonadEmail (..),
    MockEmailT (..),
    runMockEmailT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Philia093.Notify (MonadNotify (..))
import Philia093.Processor (MonadProcessor (..))
import Philia093.Types

-- | Typeclass for email operations - the Haskell way!
class (Monad m) => MonadEmail m where
  fetchEmails :: m [Email]
  sendEmail :: Email -> m ()
  markAsRead :: EmailId -> m ()

-- | Mock implementation using transformer for testing
newtype MockEmailT m a = MockEmailT {unMockEmailT :: ReaderT EmailConfig m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)
-- TODO: Consider using `via` deriving and explicit instance definitions for clarity.
-- Also, `ReaderT EmailConfig` suggests this transformer's only purpose is config access.
-- This could be replaced with `MonadReader EmailConfig m` constraint instead of a transformer.
-- Pattern:
--   newtype MockEmailT m a = MockEmailT { runMockEmailT :: m a }
--   instance MonadReader EmailConfig m => MonadEmail (MockEmailT m) where ...

deriving newtype instance (MonadNotify m) => MonadNotify (MockEmailT m)

deriving newtype instance (MonadProcessor m) => MonadProcessor (MockEmailT m)

deriving newtype instance (MonadError AppError m) => MonadError AppError (MockEmailT m)

-- | Run the mock email transformer
runMockEmailT :: EmailConfig -> MockEmailT m a -> m a
runMockEmailT config = flip runReaderT config . unMockEmailT

-- | Mock instance - useful for testing and development
-- TODO: The repeated `liftIO $ putStrLn` pattern suggests we need a logging abstraction.
-- Consider:
--   1. Using `MonadSay` or `MonadLog` typeclass
--   2. Using `Writer` for test assertions
--   3. Using `trace` or `Debug.Trace` for development
--
-- Also, `pure []` and `pure ()` could be replaced with `pure mempty` for consistency
-- with Monoid/Monoidal patterns:
--   fetchEmails = liftIO (logStrLn "Mock: Fetching") *> pure mempty
instance (MonadIO m) => MonadEmail (MockEmailT m) where
  fetchEmails = do
    liftIO $ putStrLn "Mock: Fetching emails"
    pure []

  sendEmail email =
    liftIO . putStrLn $ "Mock: Sending email with subject: " <> show (subject email)

  markAsRead emailId =
    liftIO . putStrLn $ "Mock: Marking email as read: " <> show emailId

-- | Real implementation would use HaskellNet / smtp-mail
-- instance MonadIO m => MonadEmail (RealEmailT m) where
--   fetchEmails = do
--     config <- MockEmailT ask
--     liftIO $ connectIMAP config >>= fetchUnread
--   ...

-- Example of lifting MonadEmail through other transformers
instance (MonadEmail m) => MonadEmail (ReaderT r m) where
  fetchEmails = lift fetchEmails
  sendEmail = lift . sendEmail
  markAsRead = lift . markAsRead
