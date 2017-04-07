{-# LANGUAGE TupleSections, DeriveFunctor #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.List (span)
import Safe (readMay)

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata go = go . fmap (cata go) . unFix

data Tok = Open | Close | Word String
  deriving (Eq, Ord, Show)

tok :: String -> [Tok]
tok [] = []
tok ('(':xs) = Open : tok xs
tok (')':xs) = Close : tok xs
tok (c : xs) = tok' c xs [] where
  tok' ' ' xs     acc = Word (reverse acc) : tok xs
  tok' ')' xs     acc = Word (reverse acc) : Close : tok xs
  tok'   c  []     acc = [Word (reverse $ c : acc)]
  tok'   c  (x:xs) acc = tok' x xs (c:acc)

tokenize :: String -> [Tok]
tokenize = filter (/= Word "") . tok

type Expr = Fix E
data E a = Lit Double
         | Add  a a
         | Sub  a a
         | Mult a a
         | Div  a a
         | Inv  a
         | Pi
         | Ee
         | Exp  a
         | Sin  a
         | Cos  a
         | Tan  a
         | Fact a
         | Log  a
         | Sqrt a
         | Pow  a a
         deriving (Eq, Ord, Show, Functor)

newtype P a = P { runP :: [Tok] -> Int -> (Maybe (a, [Tok]), Int) }

parse :: P a -> [Tok] -> (Maybe a, Int)
parse p tok = first (fst <$>) $ runP p tok 0

instance Functor p where

main :: IO ()
main = do
  str <- getLine
  return ()

