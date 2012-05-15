module Pane
  ( paneMgr
  ) where

import Parser
import Base
import FlowControllerLang
import FlowController
import Control.Monad
import HFT
import Control.Monad.State

paneMgr :: Chan (Speaker, String) -- ^commands from speaker
        -> Chan Integer           -- ^current time
        -> IO (Chan MatchTable, Chan (Speaker, String))
paneMgr reqChan timeChan = do
  tblChan <- newChan
  respChan <- newChan
  stRef <- newIORef emptyState
  let handleReq = do
        (spk, req) <- readChan reqChan
        paneM <- parseFromString spk req
        st <- readIORef stRef
        (resp, st') <- runStateT paneM st
        case resp of
          BoolResult True -> do
            writeIORef stRef st'
            writeChan respChan (spk, show resp)
          otherwise -> do
            writeChan respChan (spk, show resp)
      buildTbl = do
        now <- readChan timeChan
        st <- readIORef stRef
        let removeEndingNow sh = sh { shareReq = req }
              where req = filter
                            (\r -> reqEnd r > fromInteger now)
                            (shareReq sh)
        writeIORef stRef (st { shareTree = fmap removeEndingNow (shareTree st),
                               stateNow = now })
        writeChan tblChan (compileShareTree now (getShareTree st))
  forkIO (forever handleReq)
  forkIO (forever buildTbl)
  return (tblChan, respChan)
