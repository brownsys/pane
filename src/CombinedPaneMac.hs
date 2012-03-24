module CombinedPaneMac where

import Base
import ShareSemantics
import qualified Nettle.OpenFlow as OF
import MacLearning (macLearning)
import Parser (paneMan)

combinedPaneMac :: Chan (OF.SwitchID, Bool)
                -> Chan (OF.SwitchID, OF.PacketInfo)
                -> Chan (Speaker, String)
                -> Chan Integer
                -> IO (Chan MatchTable, Chan (Speaker, String))
combinedPaneMac switch packet paneReq time = do
  (paneTbl, paneResp) <- paneMan paneReq time
  macLearnedTbl <- macLearning switch packet
  let cmb pt mt = do
        putStrLn "PANE+MAC calculating combined table ..."
        return $ condense (unionTable (\p _ -> p) pt mt)
  combinedTbl <- unionChan cmb
                           (emptyTable, paneTbl) 
                           (emptyTable, macLearnedTbl)
  
  return (combinedTbl, paneResp)
