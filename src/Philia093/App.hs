module Philia093.App where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever, when)
import Philia093.Email
import Philia093.Notify
import Philia093.Processor
import Philia093.Types

-- | The application state containing all pluggable modules
data AppHandlers = AppHandlers
  { emailH :: EmailHandle,
    processorH :: ProcessorHandle,
    notifyH :: NotifyHandle
  }

-- | Main bot logic loop
runBot :: AppHandlers -> IO ()
runBot AppHandlers {..} = forever $ do
  putStrLn "Bot starting cycle..."
  emails <- fetchEmails emailH
  forM_ emails $ \email -> do
    res <- processEmail processorH email
    when (prHandled res) $ do
      putStrLn $ "Processed email: " ++ show (emailSubject email)
      markAsRead emailH (emailId email)
      when (prShouldNotify res) $
        notifyWebhook notifyH (prSummary res)

  putStrLn "Bot cycle complete. Sleeping for 60 seconds..."
  threadDelay (60 * 1000000) -- Sleep for 60 seconds
  putStrLn "Bot cycle complete."
