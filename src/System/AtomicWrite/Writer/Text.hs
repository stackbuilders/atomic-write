-- |
-- Module      :  System.AtomicWrite.Writer.Text
-- Copyright   :  © 2015-2017 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides functionality to dump the contents of a Text
-- to a file.

module System.AtomicWrite.Writer.Text (atomicWriteFile, atomicWriteFileWithMode) where

import           System.AtomicWrite.Internal (atomicWriteFileMaybeModeText)

import           Data.Text                   (Text)

import           Data.Text.IO                (hPutStr)

import           System.Posix.Types          (FileMode)

-- | Creates a file atomically on POSIX-compliant
-- systems while preserving permissions.
atomicWriteFile ::
  FilePath   -- ^ The path where the file will be updated or created
  -> Text    -- ^ The content to write to the file
  -> IO ()
atomicWriteFile =
  atomicWriteFileMaybeMode Nothing

-- | Creates or modifies a file atomically on
-- POSIX-compliant systems and updates permissions
atomicWriteFileWithMode ::
  FileMode      -- ^ The mode to set the file to
  -> FilePath   -- ^ The path where the file will be updated or created
  -> Text       -- ^ The content to write to the file
  -> IO ()
atomicWriteFileWithMode =
  atomicWriteFileMaybeMode . Just

-- Helper Function
atomicWriteFileMaybeMode ::
  Maybe FileMode -- ^ The mode to set the file to
  -> FilePath    -- ^ The path where the file will be updated or created
  -> Text        -- ^ The content to write to the file
  -> IO ()
atomicWriteFileMaybeMode mmode path = atomicWriteFileMaybeModeText mmode path hPutStr
