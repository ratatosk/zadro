-- | Main entry point to the application.
module Main where

findDivisors :: Int -> [Int]
findDivisors 1 = []
findDivisors n =
    let root = ceiling $ sqrt $ fromIntegral n
        prim = filter (\x -> n `mod` x == 0) [2..root - 1]
        temp = 1 : prim ++ map (\x -> n `div` x) prim in
            if (root*root == n) then root:temp else temp

sumDivisors :: Int -> Int
sumDivisors n = sum $ findDivisors n

triviality :: Int -> Double
triviality n = fromIntegral (sum $ findDivisors n) / fromIntegral n

takeUntilPrime :: [(Int, Int)] -> [(Int, Int)]
takeUntilPrime [] = []
takeUntilPrime (x@(xd, xn):xs) | xd == 1 = [x]
                               | otherwise = x : takeUntilPrime xs

leastTrivial :: Int -> Int -> Int
leastTrivial i j | i /= 1 = let withSums = map (\x -> (sumDivisors x, x)) [j, j-1..i]
                                untilPrime = takeUntilPrime withSums
                                withTriviality = map (\(a, b) -> ((fromIntegral a / fromIntegral b), b)) untilPrime in
                            snd $ minimum withTriviality
                 | otherwise = 1

-- | The main entry point.
main :: IO ()
main = do
    l <- getLine
    let i:j:_ = (map read $ words l) :: [Int]
    print $ leastTrivial i j
