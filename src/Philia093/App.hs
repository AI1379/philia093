module Philia093.App
  ( AppM,
    AppEnv (..),
    runAppM,
    runBot,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.Text as T
import Philia093.Email
import Philia093.Notify
import Philia093.Processor
import Philia093.Types

-- | Application environment - contains all configuration
-- This is the Haskell way: configuration in the Reader!
newtype AppEnv = AppEnv
  { appConfig :: AppConfig
  }

-- | Application monad stack using MTL style
-- ReaderT for configuration, ExceptT for error handling, IO for effects
type AppM = ReaderT AppEnv (ExceptT AppError IO)

-- | Run the application monad
runAppM :: AppEnv -> AppM a -> IO (Either AppError a)
runAppM env = runExceptT . flip runReaderT env

-- | Main bot logic - now uses typeclasses and constraint-based design!
-- TODO: The `forever` + do-notation pattern is common but consider using a more
-- declarative approach with `iterateM_` or `forever` with point-free composition:
--   runBot = forever $
--     liftIO (putStrLn "Bot starting cycle...")
--       *> (processEmails `catchError` handleError)
--       <&> logResult
--       <* liftIO (sleep 60)
-- This separates the "what" from the "how" more clearly.
runBot ::
  ( MonadEmail m,
    MonadProcessor m,
    MonadNotify m,
    MonadIO m,
    MonadError AppError m
  ) =>
  m ()
runBot = forever $ do
  liftIO $ putStrLn "Bot starting cycle..."

  -- Fetch and process emails with error handling
  result <- processEmails `catchError` handleError

  liftIO $ case result of
    Just msg -> putStrLn $ "Cycle completed: " <> T.unpack msg
    Nothing -> putStrLn "Cycle completed with errors"

  -- Sleep between cycles
  liftIO $ do
    putStrLn "Sleeping for 60 seconds..."
    threadDelay (60 * 1000000)

-- | Process all emails - separation of concerns
-- TODO: Consider using `foldM` or `mapM_` with better composition:
--   processEmails = 
--     fetchEmails >>= \case
--       [] -> pure $ Just "No emails to process"
--       emails -> traverse processSingleEmail emails
--                   <&> filter handled
--                   <&> length
--                   <&> formatResult
-- Or even more point-free:
--   processEmails = fetchEmails 
--     <&> \case [] -> Just "No emails"; es -> Just $ showProcessCount es
-- Using `<&>` (flipped fmap) reads left-to-right in pipeline style
processEmails ::
  ( MonadEmail m,
    MonadProcessor m,
    MonadNotify m,
    MonadIO m,
    MonadError AppError m
  ) =>
  m (Maybe T.Text)
processEmails = do
  emails <- fetchEmails

  if null emails
    then pure $ Just "No emails to process"
    else do
      -- Process each email using traverse for functional composition
      results <- traverse processSingleEmail emails
      let processedCount = length $ filter handled results
      pure . Just $ T.pack $ "Processed " <> show processedCount <> " emails"

-- | Process a single email
-- TODO: This function mixes business logic with side effects in an imperative style.
-- Consider refactoring to separate pure logic from effects:
--   processSingleEmail :: Email -> m ProcessResult
--   processSingleEmail email = 
--     liftIO (logProcessing email)
--       *> processEmail email
--       <* whenResult handled markAsReadAction
--       <* whenResult shouldNotify sendNotificationAction
--   
--   where
--     markAsReadAction = markAsRead (emailId email)
--     sendNotificationAction = sendNotification (summary result)
--     whenResult condition action result = 
--       when (condition result) action
--   
-- Or use a more declarative "effect row" pattern:
--   processSingleEmail email = do
--     result <- processEmail email
--     result `when` handled *> markAsRead (emailId email)
--     result `when` shouldNotify *> sendNotification (summary result)
--     pure result
--   where
--     when r cond = when (cond r)
processSingleEmail ::
  ( MonadEmail m,
    MonadProcessor m,
    MonadNotify m,
    MonadIO m,
    MonadError AppError m
  ) =>
  Email ->
  m ProcessResult
processSingleEmail email = do
  liftIO . putStrLn $ "Processing: " <> T.unpack (subject email)

  -- Process the email
  result <- processEmail email

  -- Handle the result
  when (handled result) $ do
    markAsRead (emailId email)

    when (shouldNotify result) $
      sendNotification (summary result)

  pure result

-- | Error handler for the bot
handleError :: (MonadNotify m, MonadIO m) => AppError -> m (Maybe T.Text)
handleError err = do
  liftIO . putStrLn $ "Error occurred: " <> show err
  sendErrorNotification err
  pure Nothing
