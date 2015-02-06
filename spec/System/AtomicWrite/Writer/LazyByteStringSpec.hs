module System.AtomicWrite.Writer.LazyByteStringSpec (spec) where

import Test.Hspec (it, describe, shouldBe, Spec)

import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)

import System.IO.Temp (withSystemTempDirectory)
import System.FilePath.Posix (joinPath)

import Data.ByteString.Lazy.Char8 (pack)

spec :: Spec
spec = describe "atomicWriteFile" $
  it "writes contents to a file" $
    withSystemTempDirectory "atomicFileTest" $ \tmpDir -> do

      let path = joinPath [ tmpDir, "writeTest.tmp" ]

      atomicWriteFile path $ pack "just testing"
      contents <- readFile path

      contents `shouldBe` "just testing"
