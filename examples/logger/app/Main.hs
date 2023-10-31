import System.AtomicWrite.Writer.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)
import Control.Concurrent (forkIO, threadDelay)

logFilePath :: FilePath
logFilePath = "app.log"

logMessage :: FilePath -> T.Text -> IO ()
logMessage logFile message = atomicWriteFile logFile message

simulateLoggingProcess :: FilePath -> IO ()
simulateLoggingProcess logFile = do
    forM_ [1..10 :: Int] $ \i -> do
        let message = "Log message " <> show i
        logMessage logFile (T.pack message)
        putStrLn $ "Logged: " <> message
        threadDelay 1000000

main :: IO ()
main = do
    TIO.writeFile logFilePath mempty

    _ <- forkIO (simulateLoggingProcess logFilePath)

    threadDelay 12000000

    putStrLn "Log file contents: "
    TIO.readFile logFilePath >>= TIO.putStrLn 
