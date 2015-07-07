module System.AtomicWrite.Writer.String (atomicWriteFile, atomicWithFile) where

import System.AtomicWrite.Internal (closeAndRename, tempFileFor)

import System.IO (Handle, hPutStr)

-- | Creates a file atomically on POSIX-compliant systems while preserving
-- permissions.
atomicWriteFile ::
  FilePath   -- ^ The path where the file will be updated or created
  -> String  -- ^ The content to write to the file
  -> IO ()
atomicWriteFile = (. flip hPutStr) . atomicWithFile

-- | A general version of 'atomicWriteFile'
atomicWithFile :: FilePath -> (Handle -> IO ()) -> IO ()
atomicWithFile f action = 
  tempFileFor f >>= \(tmpPath, h) -> action h >> closeAndRename h tmpPath f
