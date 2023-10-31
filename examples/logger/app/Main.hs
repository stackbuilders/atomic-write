{-# LANGUAGE OverloadedStrings #-}

import System.AtomicWrite.Writer.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)
import Control.Concurrent (forkIO, threadDelay)
import System.FilePath ((</>))

logFilePath :: FilePath
logFilePath = "app.log"

logMessage :: FilePath -> T.Text -> IO ()
logMessage logFile message = atomicWriteFile logFile message

simulateLoggingProcess :: FilePath -> IO ()
simulateLoggingProcess logFile = do
    forM_ [1..10] $ \i -> do
        let message = "Log message " <> T.pack (show i)
        logMessage logFile message
        putStrLn $ "Logged: " <> T.unpack message
        threadDelay 1000000

main :: IO ()
main = do
    TIO.writeFile logFilePath ""

    _ <- forkIO (simulateLoggingProcess logFilePath)

    threadDelay 12000000

    logContents <- TIO.readFile logFilePath
    putStrLn "Log File Contents:"
    TIO.putStrLn logContents