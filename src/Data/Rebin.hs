module Data.Rebin (rebin) where

-- | Rebins the binned data x1 y1 into the bins x2 by sweeping
--    across the Events defined by the X coordinates of x1 and x2.
--    Linear weight is given to each segment between Events.
rebin :: [Double] -> [Double] -> [Double] -> [Double]
rebin x1 y1 x2 = consume queue 0
  where queue = merge (zipWith X1 x1 y1) (map X2 x2)

-- | We use a "sweep" style algorithm which must be able to
--    distinguish between points of the old and the new datasets.
data Event = X2 Double | X1 Double Double deriving (Eq, Show)

-- | Events must be orderable by their X values
instance Ord Event where
    compare e1 e2 = case compare (x e1) (x e2) of
                        EQ -> if isX2 e1 then LT else GT
                        a  -> a
      where x :: Event -> Double
            x (X1 d _) = d
            x (X2 d)   = d
            isX2 :: Event -> Bool
            isX2 (X2 _) = True
            isX2 _      = False

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
consume [X1 _ t] remT = [t + remT]
consume [_]      remT = [remT]
-- Case: X1,X1 ; just add the tallies into remT
consume (X1 _ t:e@(X1 _ _):es) remT
    = consume (e:es) (remT + t)
-- Case: X1,X2 ; dump remT, weight t linearly, add new event X1 at X2's loc.
consume (X1 x1l t:(X2 x2):es) remT
    | x1l == x2 = consume (X1 x1l t : es) remT -- border case: X1 and X2 overlap
    | otherwise = (t*f + remT) : consume ((X1 x2 ((1-f)*t)):es) 0
  where
    f = (x2-x1l)/(findNextXVal es - x1l)
-- Case: X2,X1 ; just move on, drop the X2
consume (X2 _:e@(X1 _ _):es) remT
    = consume (e:es) remT
-- Case: X2,X2 ; no tallies
consume (X2 _:e@(X2 _):es) _
    = 0 : consume (e:es) 0

findNextXVal :: [Event] -> Double
findNextXVal [] = error "empty list"
findNextXVal (X1 x _ : _) = x
findNextXVal (_:es) = findNextXVal es