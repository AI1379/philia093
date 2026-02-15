{- HLINT ignore "Use lambda-case" -}
module Philia093.Email.IMAPService
  ( MonadIMAP (..),
    HasIMAPConfig (..),
    IMAPT (..),
    IMAPConnection,
    runIMAP,
    withConnection,
    defaultSearchCriteria,
    runWithConnection,
  )
where

import Control.Exception (bracket)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Philia093.Email.EmailTypes

-- | Opaque connection handle for IMAP
-- In a real implementation, this would wrap HaskellNet's IMAPConnection
data IMAPConnection = IMAPConnection
  { connHost :: String,
    connMailbox :: Maybe Mailbox
    -- Internal connection state would go here
    -- For HaskellNet integration: add actual connection handle
  }
  deriving (Show)

-- | Typeclass for environments that have IMAP configuration
class HasIMAPConfig env where
  imapConfig :: env -> IMAPConfig

-- | Typeclass for IMAP operations - now resource-safe!
class (Monad m, MonadError IMAPError m) => MonadIMAP m where
  -- | Fetch unread emails from a mailbox
  fetchUnreadEmails :: Mailbox -> m [Email]

  -- | Fetch all emails from a mailbox
  fetchAllEmails :: Mailbox -> m [Email]

  -- | Mark an email as read
  markAsRead :: EmailId -> m ()

  -- | Move an email to another mailbox
  moveEmail :: EmailId -> Mailbox -> m ()

  -- | Search emails by criteria
  searchEmails :: Mailbox -> SearchCriteria -> m [Email]

-- | Search criteria for email filtering
data SearchCriteria = SearchCriteria
  { onlyUnread :: Bool,
    fromSender :: Maybe Address,
    subjectContains :: Maybe String,
    sinceDate :: Maybe String -- ISO format date
  }
  deriving (Show)

-- | Default search criteria (fetch all)
defaultSearchCriteria :: SearchCriteria
defaultSearchCriteria =
  SearchCriteria
    { onlyUnread = False,
      fromSender = Nothing,
      subjectContains = Nothing,
      sinceDate = Nothing
    }

-- | IMAP Transformer with built-in connection management
newtype IMAPT m a = IMAPT
  {unIMAPT :: ReaderT IMAPConnection (ExceptT IMAPError m) a}
  deriving (Functor, Applicative, Monad, MonadReader IMAPConnection, MonadError IMAPError, MonadIO)

-- | Run IMAP operations with automatic connection management using bracket
runIMAP :: (MonadIO m) => IMAPConfig -> IMAPT IO a -> m (Either IMAPError a)
runIMAP config action =
  liftIO
    $ bracket
      (connectIMAPIO config)
      ( \conn -> case conn of
          Right c -> disconnectIMAP c
          Left _ -> pure ()
      )
    $ \connResult -> case connResult of
      Left err -> pure $ Left err
      Right conn -> runExceptT $ runReaderT (unIMAPT action) conn

-- | Bracket-style connection management for use within ExceptT
withConnection :: (MonadIO m) => IMAPConfig -> (IMAPConnection -> ExceptT IMAPError m a) -> ExceptT IMAPError m a
withConnection config action = do
  conn <- connectIMAP config
  result <-
    action conn `catchError` \e -> do
      liftIO $ disconnectIMAP conn
      throwError e
  liftIO $ disconnectIMAP conn
  pure result

-- | Alternative: run with explicit connection (for nested operations)
runWithConnection :: (MonadIO m) => IMAPConfig -> (IMAPConnection -> IMAPT m a) -> m (Either IMAPError a)
runWithConnection config action =
  runExceptT $ withConnection config $ \conn ->
    runReaderT (unIMAPT $ action conn) conn

-- | Internal: establish IMAP connection (IO version for bracket)
connectIMAPIO :: IMAPConfig -> IO (Either IMAPError IMAPConnection)
connectIMAPIO config = do
  putStrLn $ "Connecting to IMAP: " <> show config.host <> ":" <> show config.port
  -- TODO: Real HaskellNet-SSL connection logic:
  -- case useSecurity config of
  --   SSL -> connectIMAPSSL host port
  --   STARTTLS -> connectIMAPSTARTTLS host port
  --   None -> connectIMAP host port
  -- then authenticate with user/password
  pure $
    Right $
      IMAPConnection
        { connHost = show config.host,
          connMailbox = Nothing
        }

-- | Internal: establish IMAP connection (ExceptT version)
connectIMAP :: (MonadIO m) => IMAPConfig -> ExceptT IMAPError m IMAPConnection
connectIMAP config = do
  result <- liftIO $ connectIMAPIO config
  case result of
    Left err -> throwError err
    Right conn -> pure conn

-- | Internal: close IMAP connection
disconnectIMAP :: IMAPConnection -> IO ()
disconnectIMAP conn = do
  putStrLn $ "Disconnecting from IMAP: " <> connHost conn

-- TODO: Real HaskellNet cleanup logic:
-- logout connection
-- close connection

-- | Mock implementation for development
instance (MonadIO m) => MonadIMAP (IMAPT m) where
  fetchUnreadEmails mailbox = do
    conn <- ask
    liftIO $ putStrLn $ "Fetching unread emails from: " <> show mailbox
    -- TODO: Real implementation with HaskellNet
    -- search conn "UNSEEN"
    pure []

  fetchAllEmails mailbox = do
    conn <- ask
    liftIO $ putStrLn $ "Fetching all emails from: " <> show mailbox
    -- TODO: Real implementation
    pure []

  markAsRead emailId = do
    conn <- ask
    liftIO $ putStrLn $ "Marking email as read: " <> show emailId
    -- TODO: setFlags conn [Seen]
    pure ()

  moveEmail emailId targetMailbox = do
    conn <- ask
    liftIO $ putStrLn $ "Moving email " <> show emailId <> " to " <> show targetMailbox
    -- TODO: copy then delete original
    pure ()

  searchEmails mailbox criteria = do
    conn <- ask
    liftIO $ putStrLn $ "Searching in " <> show mailbox <> " with: " <> show criteria
    -- TODO: build IMAP search query and execute
    pure []

-- | Automatic lifting through ReaderT
instance (MonadIMAP m) => MonadIMAP (ReaderT r m) where
  fetchUnreadEmails = lift . fetchUnreadEmails
  fetchAllEmails = lift . fetchAllEmails
  markAsRead = lift . markAsRead
  moveEmail emailId = lift . moveEmail emailId
  searchEmails mailbox = lift . searchEmails mailbox