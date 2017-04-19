-- |
-- Module      :  Configuration.Dotenv.Parse
-- Copyright   :  © 2015-2017 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides functionality to dump the contents of a ByteString
-- to a file.

module System.AtomicWrite.Writer.ByteString (atomicWriteFile) where

import System.AtomicWrite.Internal (closeAndRename, tempFileFor)

import Data.ByteString (ByteString, hPutStr)

-- | Creates a file atomically on POSIX-compliant systems while preserving
-- permissions.
atomicWriteFile ::
  FilePath      -- ^ The path where the file will be updated or created
  -> ByteString -- ^ The content to write to the file
  -> IO ()
atomicWriteFile f txt =
  tempFileFor f >>= \(tmpPath, h) -> hPutStr h txt >> closeAndRename h tmpPath f
