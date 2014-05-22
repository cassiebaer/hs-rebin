module Data.Rebin (rebin) where

-- | Rebins the binned data x1 y1 into the bins x2 by sweeping
--    across the Events defined by the X coordinates of x1 and x2.
--    Linear weight is given to each segment between Events.
rebin :: [Double] -> [Double] -> [Double] -> [Double]
rebin x1 y1 x2 = consume queue 0
  where queue = merge (zipWith X1 x1 y1) (map X2 x2)

-- | We use a "sweep" style algorithm which must be able to
--    distinguish between points of the old and the new datasets.
data Event = X1 Double Double | X2 Double deriving (Eq)

-- | Returns True if the Event belongs to the dataset X1
isX1 :: Event -> Bool
isX1 (X1 _ _) = True
isX1 _        = False

-- | Events must be orderable by their X values
instance Ord Event where
    compare e1 e2 = compare (x e1) (x e2)
      where x :: Event -> Double
            x (X1 d _) = d
            x (X2 d)   = d

-- | Merges two sorted event queues into one sorted event queue
merge :: [Event] -> [Event] -> [Event]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- | Takes an event queue and remaining tally (should be init. to 0)
--    and returns the rebinned tallies
consume :: [Event] -> Double -> [Double]
-- Termination clauses:
consume ((X2 _):(X1 _ _):[]) _ = error "Not enough bins"
consume (_:_:[]) _ = []
-- Case: X1,X1 ; just sum the tallies into remT
consume ((X1 _ t):e@(X1 _ _):es) remT
    = consume (e:es) (remT + t)
-- Case: X1,X2 ; grab tallies from future X1 and dump remT
consume ((X1 x1l t):e@(X2 x2):es) remT
    = (y2 + remT + t) : consume (e:es') 0
  where
    (y2,es') = grabTallies es x1l x2
-- Case: X2,X1 ; just move on
consume ((X2 _):e@(X1 _ _):es) remT
    = consume (e:es) remT
-- Case: X2,X2 ; grab tallies from future X1
consume ((X2 x2l):e@(X2 x2c):es) _
    = y2 : consume (e:es') 0
  where
    (y2,es') = grabTallies es x2l x2c

-- | Grabs tallies from a future X1 Event. Returns a new Queue with a modified
--    future X1 Event
grabTallies :: [Event] -> Double -> Double -> (Double,[Event])
grabTallies es lstX curX = (t',es')
  where
    (es1,es2) = break isX1 es
    (t',es2') = case es2 of
             ((X1 x1r t):es2'') -> let f = (curX - lstX) / (x1r - lstX)
                                   in (f * t,(X1 x1r ((1-f)*t)):es2'')
             _                  -> (0,es2)
    es' = es1 ++ es2'