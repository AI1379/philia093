module Philia093.Processor where

import Data.Text (Text)
import Philia093.Types

-- | Interface for Processing operations
data ProcessorHandle = ProcessorHandle
  { processEmail :: Email -> IO ProcessResult,
    extractPdf :: Attachment -> IO Text,
    callLLM :: Text -> IO Text
  }

-- | A simple rule-based implementation
mkSimpleProcessor :: ProcessorHandle
mkSimpleProcessor =
  ProcessorHandle
    { processEmail = \_ -> return (ProcessResult True "Processed by simple rules" True),
      extractPdf = \_ -> return "PDF Content placeholder",
      callLLM = \t -> return ("LLM Response for: " <> t)
    }
