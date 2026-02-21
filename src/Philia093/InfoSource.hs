module Philia093.InfoSource
  ( -- | Core TypeClass for information sources
    MonadInfoSource (..),
    -- | Re-export common types
    Article (..),
    ArticleId (..),
    SourceId (..),
    SourceType (..),
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Philia093.Types

-- ============================================================================
-- | InfoSource TypeClass - 统一的信息源抽象
-- ============================================================================

-- | TypeClass for information source operations
-- Allows different sources (Arxiv, Email, RSS, etc.) to be handled uniformly
class (Monad m) => MonadInfoSource m where
  -- | Fetch articles from a specific source
  -- Returns articles fetched since the last call (depends on source implementation)
  fetchArticles :: SourceId -> m [Article]

  -- | Check if an article has been processed before (for deduplication)
  -- Returns True if the article is already in the processed set
  isArticleProcessed :: Article -> m Bool

  -- | Mark an article as processed
  -- Stores the article ID to avoid re-processing
  markArticleProcessed :: Article -> m ()

  -- | Get metadata about a source
  getSourceInfo :: SourceId -> m (Maybe SourceType)

instance (MonadError AppError m, MonadIO m) => MonadInfoSource m where
  fetchArticles _sourceId = do
    liftIO $ putStrLn "Mock: Fetching articles from source"
    pure []

  isArticleProcessed _article = pure False

  markArticleProcessed article = do
    liftIO . putStrLn $ "Mock: Marking article as processed: " <> show (articleId article)

  getSourceInfo _sourceId = pure Nothing

-- | Import for use with ReaderT
-- If you're already using ReaderT BotEnv, you can extend it to implement MonadInfoSource
