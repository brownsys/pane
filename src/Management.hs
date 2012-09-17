module Management
  ( mgmtServer
  ) where

import Data.Char (isSpace)
import Base
import qualified NIB
import Debug.Trace

mgmtServer :: Chan (Speaker, Integer, String) -- ^commands from manager
           -> Chan NIB.Msg           -- ^ channel for sending messages to the NIB
           -> IO (Chan (Speaker, Integer, String))
mgmtServer reqChan nibChan  = do
  respChan <- newChan
  let handleReq = do
        (spk, clientId, req) <- readChan reqChan
        let req' = trim req
        case req' of
          "nib" -> do
            let putter = traceShow "mgmt handling request" $ (\ output -> writeChan respChan (spk, clientId, output))
            traceShow "mgmt writing to NIB channel" $ writeChan nibChan (NIB.DisplayNIB putter)
          otherwise -> do
            writeChan respChan (spk, clientId, "unknown command!")
  forkIO (forever handleReq)
  return (respChan)


trim = f . f
  where f = reverse . dropWhile isSpace
