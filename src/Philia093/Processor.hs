module Philia093.Processor
  ( MonadProcessor (..),
    analyzeArticle,
    ArticleAnalysis (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Philia093.Types

-- ============================================================================
-- | Typeclass for article/email processing operations
-- ============================================================================

-- | Typeclass for processing operations
class (Monad m) => MonadProcessor m where
  -- | Process an article from any information source
  processArticle :: Article -> m ProcessResult

  -- | Extract PDF content from an attachment (useful for arxiv papers)
  extractPdfContent :: Attachment -> m Text

  -- | Summarize text with LLM
  summarizeWithLLM :: Text -> m Text

  -- | Calculate relevance score (0-1) for an article
  calculateRelevance :: Article -> Text -> m Float

-- ============================================================================
-- | Article Analysis Helper
-- ============================================================================

data ArticleAnalysis = ArticleAnalysis
  { hasContent :: Bool,
    contentLength :: Int,
    needsProcessing :: Bool,
    isRelevant :: Bool
  }
  deriving (Show)

-- | Pure analysis function - separation of concerns
analyzeArticle :: Article -> ArticleAnalysis
analyzeArticle article =
  ArticleAnalysis
    { hasContent = not (T.null (excerpt article)),
      contentLength = T.length (excerpt article),
      needsProcessing = not (T.null (excerpt article)),
      isRelevant = hasRelevantContent article
    }
  where
    -- Simple heuristic for relevance
    hasRelevantContent a = 
      let titleWords = T.words (title a)
          excerptLength = T.length (excerpt a)
       in length titleWords > 2 && excerptLength > 50

-- ============================================================================
-- | Default Instance
-- ============================================================================

-- | Instance for the main BotM monad stack
-- With the ReaderT pattern, processor can access LLM config via ReaderT
instance (MonadIO m, MonadError AppError m) => MonadProcessor (ReaderT BotEnv m) where
  processArticle article = do
    env <- ask
    let _config = botLLMConfig env
    let analysis = analyzeArticle article
    liftIO . putStrLn $ "Processing article: " <> T.unpack (title article)

    currentTime <- liftIO getCurrentTime
    pure $
      ProcessResult
        { article = article,
          handled = needsProcessing analysis,
          summary = "Processed: " <> title article,
          relevanceScore = if isRelevant analysis then 0.8 else 0.3,
          shouldNotify = hasContent analysis,
          error = Nothing,
          processedAt = currentTime
        }

  extractPdfContent attachment = do
    liftIO . putStrLn $ "Extracting PDF: " <> T.unpack (fileName attachment)
    pure $ "PDF content from: " <> fileName attachment

  summarizeWithLLM text = do
    env <- ask
    let _config = botLLMConfig env
    liftIO . putStrLn $ "Calling LLM for text length: " <> show (T.length text)
    pure $ "Summary: " <> T.take 50 text <> "..."

  calculateRelevance _article _summary = do
    liftIO $ putStrLn "Mock: Calculating relevance score"
    pure 0.75
