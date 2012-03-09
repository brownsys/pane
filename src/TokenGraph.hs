module TokenGraph
  ( TokenGraph
  , drain
  , pour
  , new
  , tokensAt
  ) where

import Base
import Debug.Trace

data Event = Event {
  eventTime :: Limit,
  numTokens :: Limit,
  drainRate :: Integer
}

instance Show Event where
  show (Event t n d) = show (t,n,d)

data TokenGraph = TokenGraph {
  history  :: [Event], -- ^invariant: strictly increasing eventTimes
  fillRate :: Integer,
  minDrain :: Integer,
  maxDrain :: Limit,
  capacity :: Limit
} deriving Show

new :: Integer
    -> Integer
    -> Limit
    -> Limit
    -> TokenGraph
new fill minDrain maxDrain cap
  | fill >= 0 && 
    fromInteger minDrain <= maxDrain &&
    minDrain >= 0 &&
    maxDrain >= 0 &&
    cap >= fromInteger 0 = 
      -- We can either insert this initial event here, or in pour
      TokenGraph [Event (fromInteger 0) (fromInteger 0) 0] 
                 fill minDrain maxDrain cap
  | otherwise = 
      error ("TokenGraph.new " ++ show fill ++ " " ++ show minDrain ++ " " ++
             show maxDrain ++ " " ++ show cap)

-- |'pour n g' pours 'n' tokens into the 'TokenGraph' 'g', possibly exceeding
-- the capacity of the graph.
pour :: Integer
     -> TokenGraph
     -> TokenGraph
pour n graph = case history graph of
  [] -> error "TokenGraph.pour : empty history"
  (hd:tl) -> graph { history = updateNumToks 0
                                             (fillRate graph)
                                             (capacity graph)
                                             (hd':tl) }
               where hd' = hd { numTokens = numTokens hd + fromInteger n }
   
-- |'tokensAt t g' returns the number of tokens in 'g' at time 't'. It signals
-- an error if 't' is less than the starting time of 'g''s history.
tokensAt :: Limit     
         -> TokenGraph
         -> Limit
tokensAt t (TokenGraph{history=hist, fillRate=fill, capacity=cap}) = 
  case hist of
    [] -> error "TokenGraph.tokensAt : empty history"
    hd:tl -> at hd tl where
               at prev [] = 
                    toksAt fill cap prev t
               at prev (evt:rest)  
                    | eventTime evt == t = numTokens evt
                    | eventTime evt > t  = toksAt fill cap prev t
                    | otherwise          = at evt rest


isValidEvent :: TokenGraph -> Event -> Bool
isValidEvent (TokenGraph _ _ minDrain maxDrain cap) (Event _ tokens drain)  =
  minDrain <= drain &&
  fromInteger drain <= maxDrain &&
  tokens >= fromInteger 0 &&
  tokens <= cap

insEvent :: Event -> [Event] -> [Event]
insEvent event [] = [event]
insEvent event (hd:tl)
  | eventTime event < eventTime hd = event:hd:tl
  | eventTime event > eventTime hd = hd:(insEvent event tl)
  | otherwise                      = merged:tl 
      where merged = Event (eventTime hd) 
                           (error "did not recompute numTokens")
                           (drainRate event + drainRate hd)

-- |'toksAt fill cap prevEvt tm' calculates the number of tokens at time 'tm',
-- given that 'prevEvt' represents the last point in the history where the
-- drain rate changes.
toksAt fillRate cap (Event prevTm prevToks prevDrain) tm =
  min cap 
      (prevToks
       + ((tm - prevTm) * (fromInteger (fillRate - prevDrain))))

updateNumToks :: Limit 
              -> Integer
              -> Limit
              -> [Event]
              -> [Event]
updateNumToks _ _ _ [] = []
updateNumToks initNumToks fill cap (hd:tl) = scanl upd hd' tl where
  hd' = hd { numTokens = initNumToks }
  upd prevEvt (Event tm _ drain) = 
    Event tm (toksAt fill cap prevEvt tm) drain

drain :: Integer -- ^start time
      -> Limit   -- ^end time
      -> Integer -- ^tokens drained per tick
      -> TokenGraph
      -> Maybe TokenGraph
drain startTime endTime rate gr@(TokenGraph hist fill minDrain maxDrain cap) =
  case fromInteger startTime < endTime of
    False -> Nothing
    True -> 
      let start = Event (fromInteger startTime) 
                         (error "missing start") (fromInteger rate)
          end   = Event endTime (error "missing end") 0
          hist' = updateNumToks 0 fill cap (insEvent start (insEvent end hist))
         in case all (isValidEvent gr) hist' of
              True -> Just (gr { history = hist' })
              False -> Nothing
