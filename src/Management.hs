module Management
  ( mgmtServer
  ) where

import Data.Char (isSpace)
import Base
import qualified NIB

mgmtServer :: Chan (Speaker, Integer, String) -- ^commands from manager
           -> Chan NIB.Msg          -- ^ channel for sending messages to the NIB
           -> PaneConfig            -- ^ current server configuration
           -> IO (Chan (Speaker, Integer, String))
mgmtServer reqChan nibChan config = do
  respChan <- newChan
  let handleReq = do
        (spk, clientId, req) <- readChan reqChan
        let req' = trim req
        case req' of
          "nib" -> do
            let putter = (\output -> writeChan respChan (spk, clientId, output))
            writeChan nibChan (NIB.DisplayNIB putter)
          -- TODO(adf): add commands to change the logging levels
          -- setupLogging config
          otherwise -> do
            writeChan respChan (spk, clientId, "unknown command!")
  forkIO (forever handleReq)
  return (respChan)


trim = f . f
  where f = reverse . dropWhile isSpace
