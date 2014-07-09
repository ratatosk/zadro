{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List
import           Data.Maybe
import           Debug.Trace

type Layer = UArray (Int, Int) Int

toIntList :: BS.ByteString -> [Int]
toIntList s = let trimmed = BS.dropWhile isSpace s
                  (i, rest) = fromJust $ BS.readInt trimmed
              in i : toIntList rest

iter2d :: Int -> Int -> (Int -> Int -> b) -> [b]
iter2d m n f = run 1 1
    where
        run r c | r == m && c == n = [p]
                | c < n = p : run r (c + 1)
                | r < m && c == n = p : run (r + 1) 1
            where p = f r c

parse :: BS.ByteString -> (Int, Int, Layer)
parse s = let (m:n:bricks) = toIntList s
              idxs = iter2d m n (,)
              b = array ((1, 1), (m, n)) $ zip idxs bricks
          in (m, n, b)

dbg :: Show a => a -> a
dbg a = traceShow a a

solve :: Int -> Int -> Layer -> Layer
solve m n l = accumArray (flip const) 1 ((1, 1), (m, n)) next
    where next = concat $ iter2d (m `div` 2) (n `div` 2) mix
          mix r c = let brickNo = n * (r - 1) + c * 2 - 1
                        curR = 2 * r - 1
                        curC = 2 * c - 1
                        cur = (curR, curC)
                        right = (curR, curC + 1)
                        down = (curR + 1, curC)
                        diag = (curR + 1, curC + 1)
                    in if (l ! cur /= l ! right && l ! down /= l ! diag)
                       then [(cur, brickNo), (right, brickNo), (down, brickNo + 1), (diag, brickNo + 1)]
                       else [(cur, brickNo), (right, brickNo + 1), (down, brickNo), (diag, brickNo + 1)]

printLayer :: Int -> Int -> Layer -> [String]
printLayer m n l = map printLevel [1..m]
    where printLevel r = concat $ intersperse " " $ map (\c -> show $ l ! (r, c)) [1..n]

testInput :: BS.ByteString
testInput = "2 4\n2 1 1 4\n2 3 3 4\n"

-- | The main entry point.
main :: IO ()
main = do
    input <- BS.getContents
    let (m, n, bricks) = parse input
    let res = solve m n bricks
    forM_ (printLayer m n res) putStrLn
