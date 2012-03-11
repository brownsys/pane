module TokenGraph
  ( TokenGraph
  , isConstraintsContained
  , drain
  , pour
  , new
  , tokensAt
  , unconstrained
  , graph
  ) where

import Base
import Debug.Trace

data Event = Event {
  eventTime :: Limit,
  numTokens :: Limit,
  drainRate :: Integer
} deriving Eq

instance Show Event where
  show (Event t n d) = show (t,n,d)

data TokenGraph = TokenGraph {
  history  :: [Event], -- ^invariant: strictly increasing eventTimes
  fillRate :: Integer,
  minDrain :: Limit,
  maxDrain :: Limit,
  capacity :: Limit
} deriving (Show, Eq)

isConstraintsContained :: TokenGraph -> TokenGraph -> Bool
isConstraintsContained (TokenGraph _ fill minDrain maxDrain cap)
                       (TokenGraph _ fill' minDrain' maxDrain' cap') =
  maxDrain <= maxDrain' &&
  (minDrain' == NoLimit || minDrain >= minDrain') &&
  (cap <= cap')
  -- TODO(adf): constraints on fill?

unconstrained = TokenGraph [Event 0 NoLimit 0] 0 NoLimit NoLimit NoLimit

graph (TokenGraph{history=hist, maxDrain=md}) = map f hist
  where f (Event t n d) = (t, md - fromInteger d, n)

new :: Integer
    -> Limit
    -> Limit
    -> Limit
    -> TokenGraph
new fill minDrain maxDrain cap
  | fill >= 0 && 
    (minDrain == NoLimit || minDrain <= maxDrain) &&
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
-- TODO(adf): should pour from one bucket to another, also pour min of
-- n and and (capacity - numTokens hd) ... also, should be at time 0
pour :: Integer
     -> TokenGraph
     -> TokenGraph
pour n graph = case history graph of
  [] -> error "TokenGraph.pour : empty history"
  (hd:tl) -> graph { history = updateNumToks (numTokens hd')
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
                    extrapolate fill cap prev t
               at prev (evt:rest)  
                    | eventTime evt == t = numTokens evt
                    | eventTime evt > t  = extrapolate fill cap prev t
                    | otherwise          = at evt rest


isValidEvent :: TokenGraph -> Event -> Bool
isValidEvent (TokenGraph _ _ minDrain maxDrain cap) (Event _ tokens drain)  =
  (minDrain == NoLimit || drain == 0 || minDrain <= fromInteger drain) &&
  fromInteger drain <= maxDrain &&
  tokens >= fromInteger 0 &&
  tokens <= cap

updateDrain :: Limit   -- ^start time
            -> Limit   -- ^end time
            -> Integer -- ^additional drain
            -> [Event] -- ^history
            -> [Event] -- ^updated history
updateDrain start end drain hist = ins 0 hist
  where ins _ [] = [Event start (error "missing startToks") drain,
                    Event end (error "missing endToks") 0]
        ins prevDrain (hd:tl)
          | eventTime hd < start  = hd:(ins (drainRate hd) tl)
          | eventTime hd == start = 
            let hdDrain = drainRate hd
              in (Event start undefined (hdDrain + drain)):(insEnd hdDrain tl)
          | otherwise             = evt:(insEnd prevDrain (hd:tl))
              where evt    = Event start undefined drain'
                    drain' = prevDrain + drain
        insEnd prevDrain [] = [Event end undefined 0]
        insEnd prevDrain (hd:tl)
          | eventTime hd < end  = 
              let hdDrain = drainRate hd
                in (hd { drainRate = drain + hdDrain }):(insEnd hdDrain tl)
          | eventTime hd == end = hd:tl
          | otherwise           = evt:hd:tl
              where evt = Event end undefined prevDrain

-- |'extrapolate fill cap prevEvt tm' calculates the number of tokens at time 'tm',
-- given that 'prevEvt' represents the last point in the history where the
-- drain rate changes.
extrapolate fillRate cap (Event prevTm prevToks prevDrain) tm =
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
    Event tm (extrapolate fill cap prevEvt tm) drain

drain :: Integer -- ^start time
      -> Limit   -- ^end time
      -> Integer -- ^tokens drained per tick
      -> TokenGraph
      -> Maybe TokenGraph
drain startTime endTime rate gr@(TokenGraph hist fill minDrain maxDrain cap) =
  case hist of
    (Event histStartTm histStartToks _):tl -> 
       case fromInteger startTime < endTime && 
            fromInteger startTime >= histStartTm of
         False -> Nothing
         True -> 
           let hist' = updateNumToks histStartToks fill cap 
                         (updateDrain (fromInteger startTime) endTime rate hist)
             in case all (isValidEvent gr) hist' of
                  True -> Just (gr { history = hist' })
                  False -> Nothing
    otherwise -> error "TokenGraph.drain: empty history"
