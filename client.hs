import System.IO
import System.Environment
import Network
import Control.Concurrent
import Control.Concurrent.STM

main :: IO ()
main = do
  (hostname,port) <- getArgs >>= \(h:p:_) -> return (h,(PortNumber . fromInteger . read) p)
  putStr "Enter Your Name: "
  hFlush stdout
  handle <- connectTo hostname port
  hSetNewlineMode handle $ NewlineMode LF LF
  forkIO $ listenChat handle
  sequence_ $ repeat (getLine >>= hPutStrLn handle >> hFlush handle)

listenChat :: Handle -> IO ()
listenChat handle = sequence_ $ repeat (hGetLine handle >>= putStrLn)
