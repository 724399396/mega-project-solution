module Main where

import System.Environment (getArgs)
import Control.Monad (mplus)
import Data.List (nub,sort)

change :: Int -> [Int] -> [[Int]]
change 0 _ = [[]]
change _ [] = []
change amount coins = nub $ map sort $ do
  coin <- filter (<= amount) coins
  (change (amount - coin) coins >>= \sol -> return $ (coin:sol)) `mplus` (change amount (filter (/=coin) coins))

main :: IO ()
main = do
  args <- getArgs
  let amount = read $ args!!0
      total = read $ args!!1
  print $ head $ change (total - amount) [25, 10, 5, 1]
