module Main where

import System.Environment (getArgs)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibN :: Int -> Integer
fibN n = fibs !! n

main :: IO ()
main = do
  args <- getArgs
  let n = read $ args!!0
  putStrLn $ show $ fibN n
