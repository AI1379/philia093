module Philia093.Email where

import Data.Text (Text)
import Philia093.Types

-- | Interface for Email operations
data EmailHandle = EmailHandle
  { fetchEmails :: IO [Email],
    sendEmail :: Email -> IO (),
    markAsRead :: Text -> IO ()
  }

-- | A placeholder implementation (Mock)
mkMockEmailHandle :: EmailHandle
mkMockEmailHandle =
  EmailHandle
    { fetchEmails = return [],
      sendEmail = \_ -> putStrLn "Mock: Sending Email",
      markAsRead = \_ -> putStrLn "Mock: Marking as read"
    }

-- | Real implementation would use HaskellNet / smtp-mail
-- mkRealEmailHandle :: Config -> IO EmailHandle
-- mkRealEmailHandle config = ...
