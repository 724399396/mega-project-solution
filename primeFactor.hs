module Main where
import Control.Monad (join, ap)
import System.Environment (getArgs)

isPrime :: Int -> Bool
isPrime = ap (all. ((/=0).) . mod) (flip takeWhile primes . (.join (*)) . (flip (<=)))

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

primeFactor :: Int -> [Int]
primeFactor x = filter (\y -> mod x y == 0) $ takeWhile (<=x) primes

main :: IO ()
main = do
  args <- getArgs
  print $ primeFactor (read $ args!!0)
