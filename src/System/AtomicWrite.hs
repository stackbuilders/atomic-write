-- |
-- Module      : System.AtomicWrite
-- Copyright   : (c) 2015 Stack Builders Inc.
--
-- License     : MIT
-- Maintainer  : justin@stackbuilders.com
-- Stability   : experimental
-- Portability : GHC
--
-- A library for atomically modifying files while preserving permissions
--

module System.AtomicWrite (atomicWriteFile) where

import System.Directory (renameFile, doesFileExist)
import System.FilePath.Posix (takeDirectory)
import System.IO
  (Handle, openTempFile, openTempFileWithDefaultPermissions, hPutStr, hClose)

import System.Posix.Files (setFileMode, getFileStatus, fileMode)


-- | Creates a file atomically on POSIX-compliant systems while preserving
-- permissions.
atomicWriteFile ::
  FilePath   -- ^ The path where the file will be updated or created
  -> String  -- ^ The content to write to the file
  -> IO ()
atomicWriteFile f txt = do
  (temppath, h) <- tempFileFor f

  hPutStr h txt
  hClose h

  renameFile temppath f


-- | Returns a temporary file with permissions correctly set.  chooses
-- either previously-set permissions if the file that we're writing
-- to existed, or permissions following the current umask.
tempFileFor :: FilePath -> IO (FilePath, Handle)
tempFileFor originalFilePath = do
  let targetDirectory = takeDirectory originalFilePath

  doesFileExist originalFilePath >>=
    tmpFile originalFilePath targetDirectory "atomic.write"

  where

    tmpFile :: FilePath -> FilePath -> String -> Bool -> IO (FilePath, Handle)
    tmpFile originalPath workingDirectory template previousExisted =

      if previousExisted then do
        (temppath, handle) <- openTempFile workingDirectory template

        oldStat <- getFileStatus originalPath

        setFileMode temppath $ fileMode oldStat

        return (temppath, handle)

      else
        openTempFileWithDefaultPermissions workingDirectory template
