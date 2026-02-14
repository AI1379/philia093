module Philia093.LLM.LLMProvider
  ( MonadLLM (..),
    HasLLMConfig (..),
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Text
import Philia093.LLM.LLMTypes

-- | Typeclass for LLM operations - much more Haskell!
class (Monad m, MonadError LLMError m) => MonadLLM m where
  -- | Synchronous completion request
  complete :: LLMRequest -> m LLMResponse

  -- | Stream completion (optional, can throw NotImplemented)
  completeStreaming :: LLMRequest -> (Text -> m ()) -> m LLMResponse
  completeStreaming req _ = complete req -- Default implementation

-- | Typeclass for environments that have LLM configuration
-- This is the Haskell way of "baking in" config!
class HasLLMConfig env where
  llmConfig :: env -> LLMConfig

-- | Automatic lifting through ReaderT
instance (MonadLLM m) => MonadLLM (ReaderT r m) where
  complete = lift . complete
  completeStreaming req handler = ReaderT $ \r ->
    completeStreaming req (\txt -> runReaderT (handler txt) r)