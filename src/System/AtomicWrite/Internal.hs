module System.AtomicWrite.Internal (closeAndRename, tempFileFor) where

import System.Directory (doesFileExist, renameFile)
import System.Posix.Files (setFileMode, getFileStatus, fileMode)
import System.FilePath.Posix (takeDirectory)

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
