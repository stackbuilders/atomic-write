module System.AtomicWrite.Writer.Text (atomicWriteFile) where

import System.AtomicWrite.Internal (closeAndRename, tempFileFor)

import Data.Text (Text)

import Data.Text.IO (hPutStr)

-- | Creates a file atomically on POSIX-compliant systems while preserving
-- permissions.
atomicWriteFile ::
  FilePath   -- ^ The path where the file will be updated or created
  -> Text    -- ^ The content to write to the file
  -> IO ()
atomicWriteFile f txt =
  tempFileFor f >>= \(tmpPath, h) -> hPutStr h txt >> closeAndRename h tmpPath f
