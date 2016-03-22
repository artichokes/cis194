module HW1.Hanoi (Peg, Move, hanoi, hanoi4) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = step3 ++ step2 ++ step1
  where
    step1 = hanoi (n-1) b a c   -- Move n-1 discs from A to B
    step2 = [(a, c)]            -- Move disc n from A to C
    step3 = hanoi (n-1) a c b   -- Move n-1 discs from B to C

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = step3 ++ step2 ++ step1
  where
    k = n - (round . (sqrt :: Double -> Double) $ fromIntegral (2*n + 1)) + 1
    step1 = hanoi4 k b a c d    -- Move k disks from A to B
    step2 = hanoi (n-k) a c d   -- Move n-k disks from A to D using pegs A,C,D
    step3 = hanoi4 k a d c b    -- Move k disks from B to D
