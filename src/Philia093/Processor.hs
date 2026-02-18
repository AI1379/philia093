module Philia093.Processor
  ( MonadProcessor (..),
    SimpleProcessorT (..),
    runSimpleProcessorT,
    analyzeEmail,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Text (Text)
import qualified Data.Text as T
import Philia093.Notify (MonadNotify (..))
import Philia093.Types

-- | Typeclass for email processing operations
class (Monad m) => MonadProcessor m where
  processEmail :: Email -> m ProcessResult
  extractPdfContent :: Attachment -> m Text
  summarizeWithLLM :: Text -> m Text

-- | Pure function to analyze email content - separation of concerns!
analyzeEmail :: Email -> EmailAnalysis
analyzeEmail email =
  EmailAnalysis
    { hasAttachments = not . null $ attachments email,
      bodyLength = T.length (maybe "" id $ bodyText email),
      needsProcessing = not (T.null $ maybe "" id $ bodyText email)
    }

data EmailAnalysis = EmailAnalysis
  { hasAttachments :: Bool,
    bodyLength :: Int,
    needsProcessing :: Bool
  }

-- | Simple rule-based processor transformer
newtype SimpleProcessorT m a = SimpleProcessorT
  {unSimpleProcessorT :: ReaderT () m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

deriving newtype instance (MonadNotify m) => MonadNotify (SimpleProcessorT m)

deriving newtype instance (MonadError AppError m) => MonadError AppError (SimpleProcessorT m)

runSimpleProcessorT :: SimpleProcessorT m a -> m a
runSimpleProcessorT = flip runReaderT () . unSimpleProcessorT

-- | Simple implementation for development
-- TODO: This instance has too much "imperative-style" logging mixed with logic.
-- Consider separating logging from processing using a Writer monad or effects:
--   instance (MonadIO m, MonadWriter [LogEntry] m) => MonadProcessor (SimpleProcessorT m) where
--     processEmail email = do
--       tell [LogInfo $ "Processing: " <> subject email]
--       pure $ processEmailPure email
--
-- Or use a Callback/Applicative pattern:
--   processEmail = \email ->
--     logProcessing email *> pure (processEmailPure email)
--
-- Also, `liftIO . putStrLn` everywhere is a code smell - consider a logging typeclass:
--   class MonadLog m where logInfo :: Text -> m ()
instance (MonadIO m) => MonadProcessor (SimpleProcessorT m) where
  processEmail email = do
    let analysis = analyzeEmail email
    liftIO . putStrLn $ "Processing email: " <> T.unpack (subject email)

    pure $
      ProcessResult
        { handled = needsProcessing analysis,
          summary = "Processed: " <> subject email,
          shouldNotify = hasAttachments analysis
        }

  extractPdfContent attachment = do
    liftIO . putStrLn $ "Extracting PDF: " <> T.unpack (fileName attachment)
    pure $ "PDF content from: " <> fileName attachment

  summarizeWithLLM text = do
    liftIO . putStrLn $ "Calling LLM for text length: " <> show (T.length text)
    pure $ "Summary: " <> T.take 50 text <> "..."

-- | Lift through other transformers
instance (MonadProcessor m) => MonadProcessor (ReaderT r m) where
  processEmail = lift . processEmail
  extractPdfContent = lift . extractPdfContent
  summarizeWithLLM = lift . summarizeWithLLM
