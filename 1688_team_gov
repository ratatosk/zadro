-- | Main entry point to the application.
module Main where

import Control.Monad (replicateM)

findEnough' :: Integer -> Integer -> Integer -> [Integer] -> Maybe Integer
findEnough' ctr acc lim [] = Nothing
findEnough' ctr acc lim (x:xs) = 
    let newAcc = acc + fromIntegral x
        newCtr = ctr + 1 in
    if newAcc > lim
       then Just newCtr
       else findEnough' newCtr newAcc lim xs

findEnough :: Integer -> [Integer] -> Maybe Integer
findEnough lim xs = findEnough' 0 0 lim xs


-- | The main entry point.
main :: IO ()
main = do
    [n, m] <- fmap (map read . words) getLine :: IO [Int]
    beers <- replicateM m (fmap read getLine) :: IO [Integer]
    let r = findEnough (fromIntegral n * 3) beers
    case r of
        Just rn -> putStrLn $ "Free after " ++ show rn ++ " times."
        Nothing -> putStrLn $ "Team.GOV!"
