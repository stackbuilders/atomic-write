-- |
-- Module      :  System.AtomicWrite.Writer.String.Binary
-- Copyright   :  © 2015-2019 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides functionality to dump the contents of a String
-- to a file in binary mode.

module System.AtomicWrite.Writer.String.Binary (atomicWriteFile, atomicWithFile, atomicWriteFileWithMode, atomicWithFileAndMode) where

import           System.AtomicWrite.Internal (closeAndRename, maybeSetFileMode,
                                              tempFileFor)

import           System.IO                   (Handle, hPutStr, hSetBinaryMode)

import           System.Posix.Types          (FileMode)

-- | Creates or modifies a file atomically on POSIX-compliant
-- systems while preserving permissions.
atomicWriteFile ::
  FilePath   -- ^ The path where the file will be updated or created
  -> String  -- ^ The content to write to the file
  -> IO ()
atomicWriteFile = (. flip hPutStr) . atomicWithFile


-- | Creates or modifies a file atomically on
-- POSIX-compliant systems and updates permissions
atomicWriteFileWithMode ::
  FileMode    -- ^ The mode to set the file to
  -> FilePath -- ^ The path where the file will be updated or created
  -> String   -- ^ The content to write to the file
  -> IO ()
atomicWriteFileWithMode mode = ( . flip hPutStr)
                             .  atomicWithFileAndMode mode

-- | A general version of 'atomicWriteFile'
atomicWithFile :: FilePath -> (Handle -> IO ()) -> IO ()
atomicWithFile = atomicWithFileAndMaybeMode Nothing

-- | A general version of 'atomicWriteFileWithMode'
atomicWithFileAndMode :: FileMode
                      -> FilePath
                      -> (Handle -> IO ())
                      -> IO ()
atomicWithFileAndMode = atomicWithFileAndMaybeMode . Just

-- | Helper function
atomicWithFileAndMaybeMode :: Maybe FileMode
                           -> FilePath
                           -> (Handle -> IO ())
                           -> IO ()
atomicWithFileAndMaybeMode mmode path action =
  tempFileFor path >>= \(tmpPath, h) -> hSetBinaryMode h True
                    >> action h
                    >> closeAndRename h tmpPath path
                    >> maybeSetFileMode path mmode
