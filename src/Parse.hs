module Parse
  ( parseToken
  , parseIf
  , parseA
  , parseDig
  , parseInt
  , parseWhiteSpace
  , mapFail
  , onFail
  , withErr
  ) where

import Control.Monad.State
import Data.Char
import Control.Applicative

parseToken :: StateT [token] (Either String) token
parseToken = StateT (\tokens ->
  case tokens of
    t : ts -> Right (t, ts)
    _ -> Left "reached empty input when expecting a token")

mapFail :: (err -> err') -> StateT tokens (Either err) result ->
           StateT tokens (Either err') result
mapFail f parse =
  StateT rawParse where
  rawParse ts = mapLeft f (runStateT parse ts)
  mapLeft _ (Right r) = Right r
  mapLeft f (Left l) = Left (f l)
  
onFail :: err' -> StateT tokens (Either err) result ->
          StateT tokens (Either err') result
onFail = mapFail . const

withErr :: StateT tokens (Either err) result -> err' ->
           StateT tokens (Either err') result 
withErr = flip onFail

parseIf :: (Show a) => (a -> Bool) ->
           String -> StateT tokens (Either String) a ->
           StateT tokens (Either String) a
parseIf pred expectedDescription parser = do
  res <- parser
  let errMsg =
        "expected: " ++ show expectedDescription ++ " but got: " ++
        show res
      check :: StateT tokens (Either String) ()
      check = guard (pred res)
  check `withErr` errMsg
  return res 

parseA :: (Eq e, Show e) => e -> StateT [e] (Either String) e
parseA token = parseIf ((==) token) (show token) parseToken

parseDig :: StateT String (Either String) Int
parseDig =
  mapFail ("parseDig error: " ++)
  . fmap (\d -> d - ord '0')
  . fmap ord
  . parseIf isDigit "numerical digit"
  $ parseToken

parseInt :: StateT String (Either String) Int
parseInt = mapFail ("parseInt error: " ++) $ do
  sign <- parseSign
  dig <- parseDig
  digs <- many parseDig
  return . sign $ foldl (\a b -> a * 10 + b) dig digs
    where
      parseSign = (parseA '-' >> return negate)
                  <|> (parseA '+' >> return id)
                  <|> (return id)

parseWhiteSpace :: StateT String (Either String) ()
parseWhiteSpace = parseIf (<= ' ') "whitespace" parseToken
                  >> return ()
