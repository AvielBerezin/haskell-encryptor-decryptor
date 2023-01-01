module Parse
  ( parseToken
  , parseIf
  , parseA
  , parseDig
  , parseInt
  , parseWhiteSpace
  ) where

import Control.Monad.State
import Data.Char
import Control.Applicative

parseToken :: StateT [token] (Either String) token
parseToken = StateT (\tokens ->
  case tokens of
    t : ts -> Right (t, ts)
    _ -> Left "reached empty input when expecting a token")

parseIf :: (a -> Bool) -> String -> StateT tokens (Either String) a ->
           StateT tokens (Either String) a
parseIf pred expectedDescription parser = do
  res <- parser
  guard (pred res)
  return res

parseA :: (Eq e, Show e) => e -> StateT [e] (Either String) e
parseA token = parseIf ((==) token) (show token) parseToken

parseDig :: StateT String (Either String) Int
parseDig = do
  c <- parseToken
  guard ('0' <= c && c <= '9')
  return (ord c - ord '0')

parseInt :: StateT String (Either String) Int
parseInt = do
  dig <- parseDig
  digs <- many parseDig
  return $ foldl (\a b -> a * 10 + b) dig digs

parseWhiteSpace :: StateT String (Either String) ()
parseWhiteSpace = do
  dig <- parseToken
  guard (dig <= ' ')
  return ()
