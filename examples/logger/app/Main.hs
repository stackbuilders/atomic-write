-- Enable the OverloadedStrings language extension to allow string literals
-- to be interpreted as Text values from the Data.Text module.
{-# LANGUAGE OverloadedStrings #-}

-- Import necessary modules and libraries.
import System.AtomicWrite.Writer.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)
import Control.Concurrent (forkIO, threadDelay)
import System.FilePath ((</>))

-- Define the file path where log messages will be stored.
logFilePath :: FilePath
logFilePath = "app.log"

-- Function to log a message to a specified file.
logMessage :: FilePath -> T.Text -> IO ()
logMessage logFile message = atomicWriteFile logFile message

-- Function to simulate a logging process by writing log messages to a file.
simulateLoggingProcess :: FilePath -> IO ()
simulateLoggingProcess logFile = do
    forM_ [1..10] $ \i -> do
        -- Create a log message as Text, incorporating the current value of 'i'.
        let message = "Log message " <> T.pack (show i)
        -- Log the message to the specified file.
        logMessage logFile message
        -- Print the message to the console for visibility.
        putStrLn $ "Logged: " <> T.unpack message
        -- Pause the execution for 1 second.
        threadDelay 1000000

main :: IO ()
main = do
    -- Clear the contents of the log file by writing an empty Text to it.
    TIO.writeFile logFilePath ""

    -- Fork a new thread to run the simulateLoggingProcess function concurrently.
    _ <- forkIO (simulateLoggingProcess logFilePath)

    -- Pause the main thread for 12 seconds to allow the logging process to run in the background.
    threadDelay 12000000

    -- Read the contents of the log file into a Text variable.
    logContents <- TIO.readFile logFilePath

    -- Print a header indicating the log file contents.
    putStrLn "Log File Contents:"

    -- Print the log file contents to the console.
    TIO.putStrLn logContents