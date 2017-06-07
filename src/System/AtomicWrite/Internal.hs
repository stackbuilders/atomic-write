-- |
-- Module      :  Configuration.Dotenv.Parse
-- Copyright   :  © 2015-2017 Stack Builders Inc.
-- License     :  MIT
--
-- Maintainer  :  Stack Builders <hackage@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides functionality to create a temporary file with correct permissions
-- atomically.

module System.AtomicWrite.Internal (closeAndRename, tempFileFor, maybeSetFileMode) where

import System.Directory (doesFileExist, renameFile)
import System.PosixCompat.Files (setFileMode, getFileStatus, fileMode)
import System.Posix.Types (FileMode)
import System.FilePath (takeDirectory)
import System.IO
  (hClose, Handle, openTempFile, openTempFileWithDefaultPermissions)

-- | Returns a temporary file with permissions correctly set. Chooses
-- either previously-set permissions if the file that we're writing
-- to existed, or permissions following the current umask.
tempFileFor ::
  FilePath -- ^ The target filepath that we will replace atomically.
  -> IO (FilePath, Handle)
tempFileFor targetFilePath =

  doesFileExist targetFilePath >>=
    tmpFile targetFilePath (takeDirectory targetFilePath) "atomic.write"

  where

    tmpFile :: FilePath -> FilePath -> String -> Bool -> IO (FilePath, Handle)
    tmpFile targetPath workingDirectory template previousExisted =

      if previousExisted then
        openTempFile workingDirectory template >>=

          \(tmpPath, handle) ->

            getFileStatus targetPath >>= setFileMode tmpPath . fileMode >>

            return (tmpPath, handle)

      else
        openTempFileWithDefaultPermissions workingDirectory template


closeAndRename :: Handle -> FilePath -> FilePath -> IO ()
closeAndRename tmpHandle tempFile destFile =
  hClose tmpHandle >> renameFile tempFile destFile

maybeSetFileMode :: FilePath -> Maybe FileMode -> IO ()
maybeSetFileMode path =
  maybe
    ( return () )
    ( \mode -> setFileMode path mode )
