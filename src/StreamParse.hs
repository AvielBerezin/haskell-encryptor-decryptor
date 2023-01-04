module StreamParse
  ( consumeInt
  ) where

import Pipes
import qualified Pipes.Prelude as P
import Data.Char

consumeMany :: Consumer Char (Either String) a ->
               Consumer Char (Either String) [a]
consumeMany consume = do
  x <- consume
  xs <- consumeMany consume
  return (x : xs)

consumeInt :: Consumer Char (Either String) Int
consumeInt = do
  dig <- consumeDig
  digs <- consumeMany consumeDig
  return (foldl (\a b -> a * 10 + b) dig digs)

consumeDig :: Consumer Char (Either String) Int
consumeDig = (P.filter (\x -> '0' <= x && x <= '9'))
             >-> (P.map ord)
             >-> (P.map (\x -> x - (ord '0')))
             >-> await
