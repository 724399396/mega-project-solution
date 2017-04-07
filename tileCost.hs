module Main where
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let c = read $ args !! 0
      w = read $ args !! 1
      h = read $ args !! 2
  print $ c * w * h
