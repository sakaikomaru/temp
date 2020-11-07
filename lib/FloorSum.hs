module FloorSum where

floorSum :: Int -> Int -> Int -> Int -> Int
floorSum n m a b
  | a >= m && b >= m = if y1 == 0 then ret1 else ret1' + floorSum y1 a1 m ((a1 - x1 `mod` a1) `mod` a1)
  | a >= m && b <  m = if y2 == 0 then ret2 else ret2' + floorSum y2 a2 m ((a2 - x2 `mod` a2) `mod` a2)
  | a <  m && b >= m = if y3 == 0 then ret3 else ret3' + floorSum y3 a3 m ((a3 - x3 `mod` a3) `mod` a3)
  | otherwise        = if y4 == 0 then ret4 else ret4' + floorSum y4 a4 m ((a4 - x4 `mod` a4) `mod` a4)
  where
    ret1  = ((n - 1) * n * (a `div` m) `div` 2) + n * (b `div` m)
    a1    = a `mod` m
    b1    = b `mod` m
    y1    = (a1 * n + b1) `div` m
    x1    = y1 * m - b1
    ret1' = ret1 + (n - (x1 + a1 - 1) `div` a1) * y1

    ret2  = (n - 1) * n * (a `div` m) `div` 2
    a2    = a `mod` m
    b2    = b
    y2    = (a2 * n + b2) `div` m
    x2    = y2 * m - b2
    ret2' = ret2 + (n - (x2 + a2 - 1) `div` a2) * y2

    ret3  = n * (b `div` m)
    a3    = a
    b3    = b `mod` m
    y3    = (a3 * n + b3) `div` m
    x3    = y3 * m - b3
    ret3' = ret3 + (n - (x3 + a3 - 1) `div` a3) * y3

    ret4  = 0
    a4    = a
    b4    = b
    y4    = (a4 * n + b4) `div` m
    x4    = y4 * m - b4
    ret4' = ret4 + (n - (x4 + a4 - 1) `div` a4) * y4

{- floorSum n m a b = sum $ [floor $ (fromIntegral ( a × i + b ) / (fromIntegral m) | i <- [0..(n - 1)]] -}