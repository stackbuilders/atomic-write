module System.AtomicWrite.Writer.ByteStringBuilderSpec (spec) where

import Test.Hspec (it, describe, shouldBe, Spec)

import System.AtomicWrite.Writer.ByteStringBuilder (atomicWriteFile)

import System.IO.Temp (withSystemTempDirectory)
import System.FilePath.Posix (joinPath)
import Data.ByteString.Builder (lazyByteString)

import Data.ByteString.Lazy.Char8 (pack)

spec :: Spec
spec = describe "atomicWriteFile" $
  it "writes contents to a file" $
    withSystemTempDirectory "atomicFileTest" $ \tmpDir -> do

      let path = joinPath [ tmpDir, "writeTest.tmp" ]

      atomicWriteFile path $ lazyByteString $ pack "just testing"
      contents <- readFile path

      contents `shouldBe` "just testing"
