module Philia093.Email.IMAPService
  ( MonadIMAP (..),
    HasIMAPConfig (..),
    IMAPT (..),
    IMAPConnection,
    runIMAP,
    withConnection,
    runWithConnection,
  )
where

import Control.Exception (SomeException, bracket, try)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text, pack, unpack)
import Data.Word (Word64)
import Network.HaskellNet.IMAP
  ( Flag (Seen),
    SearchQuery (ALLs, FROMs, UNFLAG),
    search,
    select,
  )
import Network.HaskellNet.IMAP qualified as IMAP
import Network.HaskellNet.IMAP.Connection qualified as IMAPConn
import Network.HaskellNet.IMAP.SSL (Settings (sslPort))
import Network.HaskellNet.IMAP.SSL qualified as IMAPSSL
import Philia093.Email.EmailTypes
  ( Email,
    EmailId (..),
    IMAPConfig (host, password, port, useSecurity, user),
    IMAPError (IMAPProtoError),
    Mailbox (unMailbox),
    Security (None, SSL, STARTTLS),
    authFailure,
    connError,
    mailboxError,
    messageError,
    opTimeout,
    protoViolation,
  )
import Philia093.Email.Utilities (parseEmail, setUid)
import Philia093.Utilities (returnOrThrow)
import System.Timeout (timeout)

-- | Internal representation of an IMAP connection with optional mailbox context
data IMAPConnection = IMAPConnection
  { connHost :: String,
    connMailbox :: Maybe Mailbox,
    -- Internal connection state would go here
    -- For HaskellNet integration: add actual connection handle
    conn :: Maybe IMAPConn.IMAPConnection
  }

instance Show IMAPConnection where
  show conn = "IMAPConnection { host = " <> show (connHost conn) <> ", mailbox = " <> show (connMailbox conn) <> " }"

-- | Typeclass for environments that have IMAP configuration
class HasIMAPConfig env where
  imapConfig :: env -> IMAPConfig

-- | Typeclass for IMAP operations - now resource-safe!
class (Monad m, MonadError IMAPError m) => MonadIMAP m where
  -- | Fetch email by ID
  fetchEmailById :: EmailId -> m Email

  -- | Fetch email by SearchQuery
  fetchEmailBySearchQuery :: Mailbox -> [SearchQuery] -> m [Email]

  -- | Fetch unread emails from a mailbox
  fetchUnreadEmails :: Mailbox -> m [Email]
  fetchUnreadEmails mailbox = fetchEmailBySearchQuery mailbox [FROMs "", UNFLAG Seen, ALLs]

  -- | Fetch all emails from a mailbox
  fetchAllEmails :: Mailbox -> m [Email]
  fetchAllEmails mailbox = fetchEmailBySearchQuery mailbox [ALLs]

  -- | Mark an email as read
  markAsRead :: EmailId -> m ()

  -- | Move an email to another mailbox
  moveEmail :: EmailId -> Mailbox -> m ()

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
      ( \case
          Right c -> disconnectIMAP c
          Left _ -> pure ()
      )
    $ \case
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
  connRes <- case config.useSecurity of
    SSL -> connectIMAPSSL config.host config.port
    STARTTLS -> connectIMAPSTARTTLS config.host config.port
    None -> connectIMAPNoSecurity config.host config.port
  res <- case connRes of
    Left err -> do
      putStrLn $ "Failed to connect: " <> show err
      pure $ Left err
    Right conn -> do
      putStrLn "Connection established, now authenticating..."
      loginIMAP conn config.user config.password

  pure $ case res of
    Left err -> Left err
    Right conn -> Right $ IMAPConnection (unpack config.host) Nothing (Just conn)

-- | Internal: generic connection wrapper with timeout and error handling
connectWithMethod :: String -> Text -> Int -> IO IMAPConn.IMAPConnection -> IO (Either IMAPError IMAPConn.IMAPConnection)
connectWithMethod methodName host port action = do
  connRes <-
    try (timeout (5 * 10 ^ (6 :: Int)) action) ::
      IO (Either SomeException (Maybe IMAPConn.IMAPConnection))

  case connRes of
    Left ex -> do
      putStrLn $ "Connection error: " <> show ex
      pure $ Left $ IMAPProtoError $ connError host port (show ex)
    Right Nothing -> do
      putStrLn "Connection timed out"
      pure $ Left $ IMAPProtoError $ opTimeout "IMAP connection" 5
    Right (Just conn) -> do
      putStrLn $ "Connected successfully " <> methodName
      pure $ Right conn

-- | Internal: connect without security
connectIMAPNoSecurity :: Text -> Int -> IO (Either IMAPError IMAPConn.IMAPConnection)
connectIMAPNoSecurity host port = do
  putStrLn $ "Connecting without Security to: " <> unpack host <> ":" <> show port
  connectWithMethod "without security" host port (IMAP.connectIMAPPort (unpack host) (fromIntegral port))

-- | Internal: connect with SSL
connectIMAPSSL :: Text -> Int -> IO (Either IMAPError IMAPConn.IMAPConnection)
connectIMAPSSL host port = do
  putStrLn $ "Connecting with SSL to: " <> unpack host <> ":" <> show port
  let config = IMAPSSL.defaultSettingsIMAPSSL {sslPort = fromIntegral port}
  connectWithMethod "with SSL" host port (IMAPSSL.connectIMAPSSLWithSettings (unpack host) config)

-- | Internal: connect with STARTTLS
connectIMAPSTARTTLS :: Text -> Int -> IO (Either IMAPError IMAPConn.IMAPConnection)
connectIMAPSTARTTLS host port = do
  putStrLn $ "Connecting with STARTTLS to: " <> unpack host <> ":" <> show port
  pure $ Left $ IMAPProtoError $ protoViolation "STARTTLS not implemented" (Just "Feature not yet supported in this implementation")

-- | Internal: login to IMAP server
loginIMAP :: IMAPConn.IMAPConnection -> Text -> Text -> IO (Either IMAPError IMAPConn.IMAPConnection)
loginIMAP conn user password = do
  result <-
    try (IMAP.login conn (unpack user) (unpack password)) ::
      IO (Either SomeException ())
  case result of
    Left ex -> pure $ Left $ IMAPProtoError $ authFailure user (show ex)
    Right _ -> pure $ Right conn

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
  case conn of
    IMAPConnection {conn = Just c} -> do
      putStrLn "Closing connection..."
      IMAP.logout c
    _ -> putStrLn "No active connection to close"

-- | Internal helper for maybe pattern matching in monads
-- TODO: Consider using `hoistMaybe` from `errors` package or `Control.Monad.Trans.Maybe`
-- This would be more idiomatic: hoistMaybe = MaybeT . return
-- Alternatively, use `withMaybeT` pattern or just pattern match directly inline
maybe' :: (Monad m) => m a -> (b -> m a) -> Maybe b -> m a
maybe' nothingCase justCase = \case
  Nothing -> nothingCase
  Just x -> justCase x

instance (MonadIO m) => MonadIMAP (IMAPT m) where
  -- TODO: The pattern below uses explicit pattern matching with `maybe'`.
  -- A more Haskell-like approach would use MaybeT or monadic composition:
  --   fetchEmailById emailId = do
  --     conn <- asks conn
  --     MaybeT (pure conn.conn)
  --       >>= lift . fetchEmailByIdWithConnection emailId
  --       & runMaybeT <&> fromMaybe (throwError ...)
  -- Or use `for_` / `traverse_` for its Monad instance:
  --   traverse_ (fetchEmailByIdWithConnection emailId) conn.conn
  --     >>= maybe (throwError $ IMAPError "...") pure
  fetchEmailById emailId = do
    conn <- ask
    liftIO . putStrLn $ "Fetching email by ID: " <> show emailId
    maybe'
      (throwError $ messageError (Just $ unEmailId emailId) "No active connection")
      (fetchEmailByIdWithConnection emailId)
      conn.conn
    where
      fetchEmailByIdWithConnection :: (MonadIO m) => EmailId -> IMAPConn.IMAPConnection -> IMAPT m Email
      -- TODO: Use `readMaybe` from `Text.Read` for safe parsing, then use `<$>` (functor):
      --   EmailId uidText -> imapConn -> do
      --     imapUid <- maybe (throwError $ messageError (Just uidText) "Invalid UID format") pure
      --                 $ readMaybe (unpack uidText)
      -- Or use the Either pattern:
      --   readEither (unpack uidText) `withError` \err -> messageError (Just uidText) (pack err)
      fetchEmailByIdWithConnection (EmailId uidText) imapConn = do
        let imapUid = read (unpack uidText) :: Word64
        liftIO . putStrLn $ "Fetching email UID: " <> show imapUid

        -- Fetch email headers and body as ByteString
        msgBytes <- liftIO $ IMAP.fetch imapConn imapUid

        -- TODO: Currently we make the strict one lazy for parsing, but actually
        -- Purebred prefers strict ByteString as well.
        let lazyMsgBytes = BL.fromStrict msgBytes

        -- Decode ByteString to Text and construct Email object
        returnOrThrow (messageError (Just uidText) "Failed to parse email content") $ fmap (setUid imapUid) (parseEmail lazyMsgBytes)

  -- TODO: This function has repeated pattern matching on `conn.conn`.
  -- A more Haskell-like approach would use monadic composition:
  --   fetchEmailBySearchQuery mailbox criteria = do
  --     imapConn <- asks conn.conn >>= hoistMaybe (IMAPError "No connection")
  --     liftIO $ select imapConn (unpack $ unMailbox mailbox)
  --     fetchEmailBySearchQueryWithConnection criteria imapConn
  -- Or use `MaybeT` wrapper:
  --   fetchEmailBySearchQuery mailbox criteria =
  --     MaybeT (asks $ conn.conn)
  --       >>= fetchEmailBySearchQueryWithConnection' mailbox criteria
  --       & runMaybeT
  --       >>= maybe (throwError $ IMAPError "...") pure
  fetchEmailBySearchQuery mailbox criteria = do
    conn <- ask
    liftIO . putStrLn $ "Fetching email by search criteria: " <> show criteria <> " in mailbox: " <> show mailbox

    -- Select mailbox before searching
    maybe'
      (throwError $ mailboxError (Just $ unMailbox mailbox) "select" "No active connection")
      ( \imapConn -> do
          liftIO $ putStrLn $ "Selecting mailbox: " <> show mailbox
          liftIO $ select imapConn (unpack $ unMailbox mailbox)
      )
      conn.conn

    maybe'
      (throwError $ mailboxError (Just $ unMailbox mailbox) "search" "No active connection")
      (fetchEmailBySearchQueryWithConnection criteria)
      conn.conn
    where
      fetchEmailBySearchQueryWithConnection :: (MonadIO m) => [SearchQuery] -> IMAPConn.IMAPConnection -> IMAPT m [Email]
      -- TODO: The composition `fetchEmailById . EmailId . pack . show` is good!
      -- However, consider using point-free style more consistently:
      --   fetchEmailBySearchQueryWithConnection searchQueries imapConn =
      --     liftIO (search imapConn searchQueries)
      --       >>= traverse (EmailId . pack . show >>> fetchEmailById)
      -- Also note: `>>>` (forward composition) often reads more naturally in pipeline-style code
      fetchEmailBySearchQueryWithConnection searchQueries imapConn = do
        -- Search for emails matching criteria
        uids <- liftIO $ search imapConn searchQueries
        liftIO . putStrLn $ "Found " <> show (length uids) <> " messages matching criteria"

        -- Fetch each email using traverse
        traverse (fetchEmailById . EmailId . pack . show) uids

  markAsRead emailId = do
    liftIO $ putStrLn $ "Marking email as read: " <> show emailId
    -- TODO: setFlags conn [Seen]
    pure ()

  moveEmail emailId targetMailbox = do
    liftIO $ putStrLn $ "Moving email " <> show emailId <> " to " <> show targetMailbox
    -- TODO: copy then delete original
    pure ()

-- | Automatic lifting through ReaderT
instance (MonadIMAP m) => MonadIMAP (ReaderT r m) where
  fetchEmailById = lift . fetchEmailById
  fetchEmailBySearchQuery mailbox = lift . fetchEmailBySearchQuery mailbox
  markAsRead = lift . markAsRead
  moveEmail emailId = lift . moveEmail emailId