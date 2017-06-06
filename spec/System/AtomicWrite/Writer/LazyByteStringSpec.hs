module System.AtomicWrite.Writer.LazyByteStringSpec (spec) where

import Test.Hspec (it, describe, shouldBe, Spec)

import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile, atomicWriteFileWithMode)

import System.IO.Temp (withSystemTempDirectory)
import System.FilePath.Posix (joinPath)
import System.PosixCompat.Files
  (setFileMode, setFileCreationMask, getFileStatus, fileMode)

import Data.ByteString.Lazy.Char8 (pack)

spec :: Spec
spec = do
  describe "atomicWriteFile" $ do
    it "writes contents to a file" $
      withSystemTempDirectory "atomicFileTest" $ \tmpDir -> do

        let path = joinPath [ tmpDir, "writeTest.tmp" ]

        atomicWriteFile path $ pack "just testing"
        contents <- readFile path

        contents `shouldBe` "just testing"
    it "preserves the permissions of original file, regardless of umask" $
      withSystemTempDirectory "atomicFileTest" $ \tmpDir -> do
        let filePath = joinPath [tmpDir, "testFile"]

        writeFile filePath "initial contents"
        setFileMode filePath 0o100644

        newStat <- getFileStatus filePath
        fileMode newStat `shouldBe` 0o100644

        -- New files are created with 100600 perms.
        _ <- setFileCreationMask 0o100066

        -- Create a new file once different mask is set and make sure that mask
        -- is applied.
        writeFile (joinPath [tmpDir, "sanityCheck"]) "with sanity check mask"
        sanityCheckStat <- getFileStatus $ joinPath [tmpDir, "sanityCheck"]
        fileMode sanityCheckStat `shouldBe` 0o100600

        -- Since we move, this makes the new file assume the filemask of 0600
        atomicWriteFile filePath $ pack "new contents"

        resultStat <- getFileStatus filePath

        -- reset mask to not break subsequent specs
        _ <- setFileCreationMask 0o100022

        -- Fails when using atomic mv command unless apply perms on initial file
        fileMode resultStat `shouldBe` 0o100644


    it "creates a new file with permissions based on active umask" $
      withSystemTempDirectory "atomicFileTest" $ \tmpDir -> do
        let
          filePath       = joinPath [tmpDir, "testFile"]
          sampleFilePath = joinPath [tmpDir, "sampleFile"]

        -- Set somewhat distinctive defaults for test
        _ <- setFileCreationMask 0o100171

        -- We don't know what the default file permissions are, so create a
        -- file to sample them.
        writeFile sampleFilePath "I'm being written to sample permissions"

        newStat <- getFileStatus sampleFilePath
        fileMode newStat `shouldBe` 0o100606

        atomicWriteFile filePath $ pack "new contents"

        resultStat <- getFileStatus filePath

        -- reset mask to not break subsequent specs
        _ <- setFileCreationMask 0o100022

        -- The default tempfile permissions are 0600, so this fails unless we
        -- make sure that the default umask is relied on for creation of the
        -- tempfile.
        fileMode resultStat `shouldBe` 0o100606
  describe "atomicWriteFileWithMode" $ do
    it "writes contents to a file" $
      withSystemTempDirectory "atomicFileTest" $ \tmpDir -> do

        let path = joinPath [ tmpDir, "writeTest.tmp" ]

        atomicWriteFileWithMode 0o100777 path $ pack "just testing"

        contents <- readFile path

        contents `shouldBe` "just testing"


    it "changes the permissions of a previously created file, regardless of umask" $
      withSystemTempDirectory "atomicFileTest" $ \tmpDir -> do
        let filePath = joinPath [tmpDir, "testFile"]

        writeFile filePath "initial contents"
        setFileMode filePath 0o100644

        newStat <- getFileStatus filePath
        fileMode newStat `shouldBe` 0o100644

        -- New files are created with 100600 perms.
        _ <- setFileCreationMask 0o100066

        -- Create a new file once different mask is set and make sure that mask
        -- is applied.
        writeFile (joinPath [tmpDir, "sanityCheck"]) "with sanity check mask"
        sanityCheckStat <- getFileStatus $ joinPath [tmpDir, "sanityCheck"]
        fileMode sanityCheckStat `shouldBe` 0o100600

        -- Since we move, this makes the new file assume the filemask of 0600
        atomicWriteFileWithMode 0o100655 filePath $ pack "new contents"

        resultStat <- getFileStatus filePath

        -- reset mask to not break subsequent specs
        _ <- setFileCreationMask 0o100022

        -- Fails when using atomic mv command unless apply perms on initial file
        fileMode resultStat `shouldBe` 0o100655


    it "creates a new file with specified permissions" $
      withSystemTempDirectory "atomicFileTest" $ \tmpDir -> do
        let
          filePath       = joinPath [tmpDir, "testFile"]
        atomicWriteFileWithMode 0o100606 filePath $ pack "new contents"

        resultStat <- getFileStatus filePath

        fileMode resultStat `shouldBe` 0o100606
