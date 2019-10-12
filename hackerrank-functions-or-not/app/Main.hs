module Main where

import Control.Monad
import Data.HashSet (fromList, size)
import Text.Printf

-- XXX: https://stackoverflow.com/questions/4056867/haskell-i-o-and-returning-from-a-function
leeCase :: IO [String]
leeCase = do
  n <- getLine
  tuplas <- replicateM (read n) getLine
--  print tuplas
  return tuplas

esFuncion :: [[Int]] -> Bool
esFuncion pares = size (fromList ordenadas) == length ordenadas
  where ordenadas = map (\[x,_] -> x) pares

main :: IO ()
main = do
  t <- getLine
  -- XXX: https://stackoverflow.com/questions/10285837/read-n-lines-into-a-string
  casesStr <- replicateM (read t) leeCase
  let cases = map (map (\c -> map (\x -> read x::Int) $ words c)) casesStr
--  print cases
--  print $ map (\pares -> if (esFuncion pares) then "YES" else "NO") cases
  mapM_ (printf "%s\n") (map (\pares -> if (esFuncion pares) then "YES" else "NO") cases)

