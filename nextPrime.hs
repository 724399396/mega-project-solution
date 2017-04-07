module Main where
import Control.Monad (join, ap)
import System.Environment (getArgs)

isPrime :: Int -> Bool
isPrime = ap (all. ((/=0).) . mod) (flip takeWhile primes . (.join (*)) . (flip (<=)))

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

main :: IO ()
main = mapM_ go primes where
  go prime =  getLine >> print prime 

