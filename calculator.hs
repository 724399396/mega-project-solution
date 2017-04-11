{-# LANGUAGE TupleSections, DeriveFunctor #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Monad.State
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
  tok'  c  []     acc = [Word (reverse $ c : acc)]
  tok'  c  (x:xs) acc = tok' x xs (c:acc)

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

type P a = StateT [Tok] Maybe a

parse :: P a -> [Tok] -> Maybe a
parse p tok = fst <$> runStateT p tok

pRead :: Read a => P a
pRead = do
  (Word x):rest <-get
  put rest
  lift $ readMay x
  

pSatisfy :: (String -> Bool) -> P String
pSatisfy p = do
  (Word x):rest <- get
  guard (p x)
  put rest
  return x


p :: String -> P String
p s = pSatisfy (==s)

pOpen :: P ()
pOpen = do
  Open:rest <- get
  put rest
  return ()

pClose :: P ()
pClose = do
  Close:rest <- get
  put rest
  return ()

pParens :: P a -> P a
pParens inner = pOpen *> inner <* pClose

oneOf :: [P a] -> P a
oneOf = foldr (<|>) empty

e :: P Expr
e = Fix <$> oneOf
  [ pParens sexpr
  , Lit <$> pRead
  , Pi <$ p "pi"
  , Ee <$ p "e"
  ]

sexpr = oneOf
  [ p "+"    *> (Add  <$> e <*> e)
  , p "-"    *> (Sub  <$> e <*> e)
  , p "*"    *> (Mult <$> e <*> e)
  , p "/"    *> (Div  <$> e <*> e)
  , p "inv"  *> (Inv  <$> e)
  , p "pow"  *> (Exp  <$> e)
  , p "sin"  *> (Sin  <$> e)
  , p "cos"  *> (Cos  <$> e)
  , p "tan"  *> (Tan  <$> e)
  , p "!"    *> (Fact <$> e)
  , p "ln"   *> (Log  <$> e)
  , p "sqrt" *> (Sqrt <$> e)
  , p "pow"  *> (Pow  <$> e <*> e)
  ]

fact :: Double -> Double
fact n = fromIntegral $ scanl (*) 1 [1..] !! (round n)

evalF :: E Double -> Double
evalF (Lit d     ) = d
evalF Pi          = pi
evalF Ee           = exp 1
evalF (Add  e1 e2) = e1 + e2
evalF (Sub  e1 e2) = e1 - e2
evalF (Mult e1 e2) = e1 * e2
evalF (Div  e1 e2) = e1 / e2
evalF (Inv  e    ) = recip (e)
evalF (Exp  e    ) = exp   (e)
evalF (Sin  e    ) = sin   (e)
evalF (Cos  e    ) = cos   (e)
evalF (Tan  e    ) = tan   (e)
evalF (Fact e    ) = fact  (e)
evalF (Log  e    ) = log   (e)
evalF (Sqrt e    ) = sqrt  (e)
evalF (Pow  e1 e2) = e1 ** e2
  
main :: IO ()
main = do
  l <- getLine
  case parse e (tokenize l) of
    Nothing   -> putStrLn $ "Parse error"
    Just expr -> print (cata evalF expr)

