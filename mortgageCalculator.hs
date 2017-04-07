module Main where

import System.Environment (getArgs)

fixedPay :: Double -> Double -> Int -> Double
fixedPay rate mortgage n =
  (iterate (*(1+rate)) mortgage !! n) / (fromIntegral n)

main :: IO ()
main = do
  args <- getArgs
  let mortgage = read $ args!!0
      rate = read $ args!!1
      n = read $ args!!2
  print $ fixedPay rate mortgage n
  
