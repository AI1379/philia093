module Philia093.App
  ( runBot,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Control.Monad.Except (MonadError, catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Philia093.Email
import Philia093.Email.EmailTypes (Address (..))
import Philia093.Notify
import Philia093.Processor
import Philia093.Types

-- ============================================================================

-- | Helper to convert Email to Article
-- ============================================================================

-- | Convert an Email to an Article for unified processing
emailToArticle :: Email -> IO Article
emailToArticle email = do
  now <- getCurrentTime
  pure $
    Article
      { articleId = ArticleId (unEmailId $ emailId email),
        title = subject email,
        author = case from email of
          [] -> Nothing
          (Address {name = n} : _) -> n,
        url = Nothing,
        excerpt = maybe "" id (bodyText email),
        sourceId = SourceId "email-inbox",
        sourceType = SourceEmail,
        publishedAt = date email,
        fetchedAt = now,
        metadata = mempty
      }

-- ============================================================================

-- | Main bot logic using constraint-based design
-- ============================================================================

-- | Main bot logic using constraint-based design
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

  -- Convert email to article for unified processing
  article <- liftIO $ emailToArticle email

  -- Process the article
  result <- processArticle article

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
