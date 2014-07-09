module Main where

import Control.Monad.ST
import Data.Maybe
import Data.Char
import Data.List
import Data.Graph
import qualified Data.Set as S
import Control.Monad
import Data.Array
import qualified Data.Array.Unboxed as U
import Data.Array.ST hiding (unsafeFreeze)
import Data.Array.Unsafe
import qualified Data.ByteString.Char8 as BS

type Weights = U.UArray (Int, Int) Int

loop :: Int -> STUArray s (Int, Int) Int -> STArray s Int [Int] -> [Int] -> ST s [Int]
loop 0 _ _ s = return s
loop n w g s = let ([f, t, p], rest) = splitAt 3 s in do
                   writeArray w (f, t) p
                   e <- readArray g f
                   writeArray g f (t:e)
                   loop (n-1) w g rest

parseTriples :: Int -> Int -> [Int] -> (Weights, Graph, [Int])
parseTriples n m p = runST $ do
    w <- newArray ((1, 1), (n, n)) 0 :: ST s (STUArray s (Int, Int) Int)
    g <- newArray (1, n) [] :: ST s (STArray s Int [Int])
    rest <- loop m w g p
    wf <- unsafeFreeze w
    wg <- unsafeFreeze g
    return (wf, wg, rest)

parse :: [Int] -> (Int, Int, Int, Int, Weights, Graph)
parse p = let (n:m:rest) = p
              (w, g, rest1) = parseTriples n m rest
              (s:f:_) = rest1
          in (n, m, s, f, w, g)

toIntList :: BS.ByteString -> [Int]
toIntList s = let trimmed = BS.dropWhile isSpace s
                  (i, rest) = fromJust $ BS.readInt trimmed
              in i : toIntList rest

cutDead :: Vertex -> Vertex -> Graph -> Maybe Graph
cutDead s f g =
    let fromStart = S.fromList $ reachable g s
        toEnd = S.fromList $ reachable (transposeG g) f
        workable = S.intersection fromStart toEnd
        newBounds = (S.findMin workable, S.findMax workable)
        edgeFilter (f, t) = f `S.member` workable && t `S.member` workable
        cutGraph = buildG newBounds $ filter edgeFilter $ edges g
    in if edgeFilter (s, f)
        then Just cutGraph
        else Nothing

findRoute :: Vertex -> Graph -> Weights -> Int
findRoute s g w = 
    let order = reverse $ topSort g
        paths = runSTUArray $ do
            p <- newArray (bounds g) 0
            forM_ order $ \i -> do
                let e = g ! i
                    way n = do
                        next <- readArray p n
                        return $ next + (w U.! (i, n))
                ways <- forM e way
                writeArray p i $ foldl' max 0 ways
            return p
    in paths U.! s

main :: IO ()
main = do
    input <- BS.getContents
    let (n, m, s, f, pipes, graph) = parse $ toIntList input
        modGraph = cutDead s f graph
    case modGraph of
        Nothing -> putStrLn "No solution"
        Just g -> print $ findRoute s g pipes
