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

module System.AtomicWrite.Writer.ByteString (atomicWriteFile, atomicWriteFileWithMode, atomicWriteFile', atomicWriteFileWithMode') where

import           System.AtomicWrite.Internal (closeAndRename, maybeSetFileMode,
                                              tempFileFor)

import           System.Posix.Types          (FileMode)

import           Data.ByteString             (ByteString, hPutStr)

import           System.IO                   (hSetBinaryMode)

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
atomicWriteFileWithMode mode =
  atomicWriteFileMaybeMode $ Just mode

-- | Helper function
atomicWriteFileMaybeMode ::
  Maybe FileMode
  -> FilePath      -- ^ The path where the file will be updated or created
  -> ByteString -- ^ The content to write to the file
  -> IO ()
atomicWriteFileMaybeMode mmode path text =
  tempFileFor path >>= \(tmpPath, h) -> hPutStr h text
                    >> closeAndRename h tmpPath path
                    >> maybeSetFileMode path mmode

-- | Creates or modifies a file atomically on POSIX-compliant
-- systems while preserving permissions. The file is opened in
-- binary mode.
atomicWriteFile' ::
  FilePath      -- ^ The path where the file will be updated or created
  -> ByteString -- ^ The content to write to the file
  -> IO ()
atomicWriteFile' = atomicWriteFileMaybeMode' Nothing

-- | Creates or modifies a file atomically on POSIX-compliant
-- systems and updates permissions. The file is opened in binary
-- mode.
atomicWriteFileWithMode' ::
  FileMode
  -> FilePath      -- ^ The path where the file will be updated or created
  -> ByteString -- ^ The content to write to the file
  -> IO ()
atomicWriteFileWithMode' mode =
  atomicWriteFileMaybeMode' $ Just mode

-- | Helper function for opening the file in binary mode.
atomicWriteFileMaybeMode' ::
  Maybe FileMode
  -> FilePath      -- ^ The path where the file will be updated or created
  -> ByteString -- ^ The content to write to the file
  -> IO ()
atomicWriteFileMaybeMode' mmode path text =
  tempFileFor path >>= \(tmpPath, h) -> hSetBinaryMode h True
                    >> hPutStr h text
                    >> closeAndRename h tmpPath path
                    >> maybeSetFileMode path mmode
