-- |
-- Module      :  Configuration.Dotenv.Parse
-- Copyright   :  © 2015-2017 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides functionality to dump the contents of a ByteStringBuilder
-- to a file.

module System.AtomicWrite.Writer.ByteStringBuilder (atomicWriteFile) where

import System.AtomicWrite.Internal (closeAndRename, tempFileFor)

import Data.ByteString.Builder (hPutBuilder, Builder, hPutBuilder)

import GHC.IO.Handle (hSetBinaryMode, hSetBuffering, BufferMode (BlockBuffering))

-- | Creates a file atomically on POSIX-compliant systems while preserving
-- permissions.
atomicWriteFile ::
  FilePath    -- ^ The path where the file will be updated or created
  -> Builder  -- ^ The content to write to the file
  -> IO ()
atomicWriteFile f builder = do
  (temppath, h) <- tempFileFor f

  -- Recommendations for binary and buffering are from the
  -- Data.ByteString.Builder docs:
  -- http://hackage.haskell.org/package/bytestring-0.10.2.0/docs/Data-ByteString-Builder.html#v:hPutBuilder
  hSetBinaryMode h True
  hSetBuffering h (BlockBuffering Nothing)

  hPutBuilder h builder

  closeAndRename h temppath f
