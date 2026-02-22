module Philia093.Utilities
  ( returnOrThrow,
    (??),
  )
where

import Control.Monad.Except (MonadError, throwError)
import Data.Maybe (fromMaybe)

-- | Convert a Maybe value to a monadic context that either returns the value or throws an error
-- Useful for converting Maybe types from parsers or lookups into ExceptT contexts
returnOrThrow :: (MonadError e m) => e -> Maybe a -> m a
returnOrThrow err Nothing = throwError err
returnOrThrow _ (Just a) = pure a

-- | flip fromMaybe
(??) :: Maybe a -> a -> a
(??) = flip fromMaybe