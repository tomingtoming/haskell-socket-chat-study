import System.IO
import System.Environment
import Network
import Control.Concurrent
import Control.Concurrent.STM

data ChatInfo = ChatInfo [String] [Handle]

main :: IO ()
main = do
  port <- fmap (PortNumber . fromInteger . read . head) getArgs
  sock <- listenOn port
  info <- newTVarIO $ ChatInfo [] []
  forkIO $ publishChat info
  serveChat sock info

publishChat :: TVar ChatInfo -> IO ()
publishChat info = sequence_ $ repeat $ do
  (message, handles) <- atomically $ do
    (ChatInfo messages handles) <- readTVar info
    if messages == [] then retry
    else writeTVar info (ChatInfo (tail messages) handles)
    return $ (head messages, handles)
  mapM_ (\h -> hPutStrLn h message >> hFlush h) handles

serveChat :: Socket -> TVar ChatInfo -> IO ()
serveChat sock info = sequence_ $ repeat $ do
  (handle, _, _) <- accept sock
  hSetNewlineMode handle $ NewlineMode LF LF
  atomically $ do
    ChatInfo messages handles <- readTVar info
    writeTVar info $ ChatInfo messages (handle:handles)
  forkIO $ receiveMessages handle info
  return ()

receiveMessages :: Handle -> TVar ChatInfo -> IO ()
receiveMessages handle info = do
  name <- hGetLine handle
  sequence_ $ repeat $ do
    message <- hGetLine handle
    atomically $ do
      (ChatInfo messages handles) <- readTVar info
      writeTVar info (ChatInfo ((name ++ ":" ++ message):messages) handles)
