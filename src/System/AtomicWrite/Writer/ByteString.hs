-- |
-- Module      :  System.AtomicWrite.Writer.ByteString
-- Copyright   :  © 2015-2017 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides functionality to dump the contents of a ByteString
-- to a file in text mode.

module System.AtomicWrite.Writer.ByteString (atomicWriteFile, atomicWriteFileWithMode) where

import           System.AtomicWrite.Internal (atomicWriteFileMaybeModeText)

import           System.Posix.Types          (FileMode)

import           Data.ByteString             (ByteString, hPutStr)


-- | Creates or modifies a file atomically on POSIX-compliant
-- systems while preserving permissions.
atomicWriteFile ::
  FilePath      -- ^ The path where the file will be updated or created
  -> ByteString -- ^ The content to write to the file
  -> IO ()
atomicWriteFile = atomicWriteFileMaybeMode Nothing

-- | Creates or modifies a file atomically on POSIX-compliant
-- systems and updates permissions.
atomicWriteFileWithMode ::
  FileMode
  -> FilePath      -- ^ The path where the file will be updated or created
  -> ByteString -- ^ The content to write to the file
  -> IO ()
atomicWriteFileWithMode = atomicWriteFileMaybeMode . Just

-- | Helper function
atomicWriteFileMaybeMode ::
  Maybe FileMode
  -> FilePath      -- ^ The path where the file will be updated or created
  -> ByteString -- ^ The content to write to the file
  -> IO ()
atomicWriteFileMaybeMode mmode path = atomicWriteFileMaybeModeText mmode path hPutStr

