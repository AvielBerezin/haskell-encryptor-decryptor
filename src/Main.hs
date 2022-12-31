import Data.Char (ord, chr)
import Data.Bits (xor)
import Data.Function (on)
import Data.Foldable
import Prelude hiding (repeat)
import Control.Monad
import Control.Monad.State (state, State)
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Control.Exception
import System.Random
import System.IO
import Pipes

class KeyComposed alg where
  strongCompose :: alg k1 b c -> alg k2 a b -> alg (k1,k2) a c
  compose :: alg k b c -> alg k a b -> alg k a c
  identity :: alg () a a
  strongRepeat :: Int -> alg k a a -> alg [k] a a
  repeat :: Int -> alg k a a -> alg k a a

data Encryptor k dec enc =
  Encryptor { encrypt :: k -> dec -> enc
            , decrypt :: k -> enc -> dec }

data KeyActions g s l k dec enc =
  KeyActions { generate :: g k
             , save :: k -> s ()
             , load :: l k }

data CompPair kc1 kc2 k dec enc =
  CompPair (kc1 k dec enc) (kc2 k dec enc)

instance KeyComposed Encryptor where
  strongCompose encryptor1 encryptor2 =
    Encryptor (\(k1,k2) -> encrypt encryptor1 k1 . encrypt encryptor2 k2)
              (\(k1,k2) -> decrypt encryptor2 k2 . decrypt encryptor1 k1)
  compose encryptor1 encryptor2 =
    Encryptor (\k -> encrypt encryptor1 k . encrypt encryptor2 k)
              (\k -> decrypt encryptor2 k . decrypt encryptor1 k)
  identity = Encryptor (const id) (const id)
  strongRepeat n encryptor =
    foldr strCompose (liftKey (const ()) identity) $ replicate n encryptor
    where
      strCompose encryptor1 encryptor2 =
        liftKey (\(x:xs) -> (x,xs)) $ strongCompose encryptor1 encryptor2
  repeat n encryptor =
    foldr compose (liftKey (const ()) identity) (replicate n encryptor)

instance (Applicative g, Applicative s, Applicative l) =>
         KeyComposed (KeyActions g s l) where
  strongCompose ka1 ka2 =
    KeyActions
      ((,) <$> generate ka1 <*> generate ka2)
      (\(k1,k2) -> save ka1 k1 *> save ka2 k2)
      ((,) <$> load ka1 <*> load ka2)
  compose ka _ =
    KeyActions
      (generate ka)
      (save ka)
      (load ka)
  identity =
    KeyActions
      (pure ())
      (const $ pure ())
      (pure ())
  strongRepeat n ka =
    KeyActions
      (replicateM n (generate ka))
      (traverse_ (save ka))
      (replicateM n (load ka))
  repeat = flip const

instance (KeyComposed a, KeyComposed b) => KeyComposed (CompPair a b) where
  strongCompose (CompPair enc1 ka1) (CompPair enc2 ka2) =
    CompPair (strongCompose enc1 enc2)
             (strongCompose ka1 ka2)
  compose (CompPair enc1 ka1)
          (CompPair enc2 ka2) =
    CompPair (compose enc1 enc2)
             (compose ka1 ka2)
  identity = CompPair identity identity
  strongRepeat n (CompPair a b) =
    CompPair (strongRepeat n a)
             (strongRepeat n b)
  repeat n (CompPair a b) =
    CompPair (repeat n a)
             (repeat n b)

liftKey :: (k1 -> k2) -> Encryptor k2 a b -> Encryptor k1 a b
liftKey k1ToK2 encryptor =
  Encryptor (encrypt encryptor . k1ToK2)
            (decrypt encryptor . k1ToK2)

operationAndKeyReverseEncryptor :: (k -> a -> a) -> (k -> k) -> Encryptor k a a
operationAndKeyReverseEncryptor op kRev =
  Encryptor op (op . kRev)

intCharOpFrom :: (Int -> Int -> Int) -> Int -> Char -> Char
intCharOpFrom op n c = chr $ mod (n `op` ord c) (ord maxBound + 1)

addEncrpytor :: Encryptor Int Char Char
addEncrpytor =
  operationAndKeyReverseEncryptor (intCharOpFrom (+)) negate

multEncryptor :: Encryptor Int Char Char
multEncryptor =
  operationAndKeyReverseEncryptor (intCharOpFrom (\k a -> rangedMult a k)) modReverse
  where
    bigPrime = 1114117
    modMult a b = mod (a * b) bigPrime
    rangedMult a b =
      if modMult a b <= maxBound
      then modMult a b
      else rangedMult (modMult a b) b
    modReverse = pow (bigPrime-2)
    pow 0 _ = 1
    pow 1 a = a
    pow n a | mod n 2 == 0 = pow (n `div` 2) a `modMult` pow (n `div` 2) a
            | otherwise = a `modMult` pow (n - 1) a

xorEncryptor :: Encryptor Int Char Char
xorEncryptor =
  operationAndKeyReverseEncryptor (intCharOpFrom xor) id

shift :: Functor f => Encryptor k a b -> Encryptor k (f a) (f b)
shift encryptor =
  Encryptor { encrypt = \k -> fmap $ encrypt encryptor k
            , decrypt = \k -> fmap $ decrypt encryptor k }

shiftUp :: Encryptor Int String String
shiftUp = shift addEncrpytor

shiftMult :: Encryptor Int String String
shiftMult = shift multEncryptor

shiftXor :: Encryptor Int String String
shiftXor = shift xorEncryptor


mapGenerate :: (g k -> g' k) -> KeyActions g s l k dec enc ->
               KeyActions g' s l k dec enc
mapGenerate fromGen keyActions =
  KeyActions (fromGen . generate $ keyActions)
             (save keyActions)
             (load keyActions)

mapSave :: (s () -> s' ()) -> KeyActions g s l k dec enc ->
           KeyActions g s' l k dec enc
mapSave fromSave keyActions =
  KeyActions (generate keyActions)
             (fromSave . save keyActions)
             (load keyActions)

mapLoad :: (l k -> l' k) -> KeyActions g s l k dec enc ->
           KeyActions g s l' k dec enc
mapLoad fromLoad keyActions =
  KeyActions (generate keyActions)
             (save keyActions)
             (fromLoad . load $ keyActions)

gen :: (Random a, RandomGen g) => State g a
gen = state random

fileReader :: Handle -> Producer Char IO ()
fileReader fh = do
  isEOF <- lift . hIsEOF $ fh
  unless isEOF $ do
    lift . hGetChar $ fh
    fileReader fh

fileWriter :: Handle -> Consumer Char IO ()
fileWriter fh = do
  c <- await
  lift . hPutChar fh $ c
  fileWriter fh

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

parseSpace :: StateT String (Either String) ()
parseSpace = do
  dig <- parseToken
  guard (dig == ' ')
  return ()

intActions :: KeyActions
                (State StdGen)
                (ReaderT Handle IO)
                (StateT String (Either String))
                Int
                dec enc
intActions =
  KeyActions
    gen
    (\k -> ReaderT (flip hPrint k))
    (many parseSpace *> parseInt)

readA :: (Eq a) => a -> ReaderT a Maybe a
readA val = do
  a <- ask
  guard (a == val)
  return val

performEncryption :: (Monad m) =>
                     (enc -> m ()) ->
                     (m dec) ->
                     CompPair (KeyActions m m m) Encryptor k dec enc ->
                     m ()
performEncryption consumeEnc dec (CompPair ka encryptor) = do
  key <- generate ka
  dec <- dec
  consumeEnc . encrypt encryptor key $ dec

main :: IO ()
main = performEncryption writeEnc dec alg where
  alg = (CompPair (mapGenerate fromGen .
                   mapSave fromSave .
                   mapLoad fromLoad $
                   intActions)
          shiftUp)
  dec = requestFileToReadDecrypted >>= hGetContents
  enc = requestFileToReadEncrypted >>= hGetContents

fromGen :: State StdGen a -> IO a
fromGen gen = pure $ evalState gen (mkStdGen 137)

fromSave :: ReaderT Handle IO () -> IO ()
fromSave saving = writeHandleFromUser >>= runReaderT saving

fromLoad :: StateT String (Either String) k -> IO k
fromLoad loading = do
  h <- readHandleFromUser
  str <- hGetContents h
  either fail
    parsedToIO
    (runStateT loading str)
  where
    parsedToIO (r, "") = return r
    parsedToIO (r, remains) = fail ("got remains after key parse: " ++ show remains)

writeEnc :: String -> IO ()
writeEnc enc = do
  fh <- requestFileToWriteEncrypted
  hPutStr fh enc

requestFileToWriteEncrypted :: IO Handle
requestFileToWriteEncrypted = do
  putStrLn "please enter a path for the encrypted file to be saved at"
  path <- getLine
  fh <- try $ openFile path WriteMode
  case fh of
    Left e -> do
      putStrLn $ "could not open a file handle for writing into " ++
                 show path
      putStrLn $ "because: " ++ show (e :: IOException)
      requestFileToWriteEncrypted
    Right fh -> return fh

requestFileToReadDecrypted :: IO Handle
requestFileToReadDecrypted = requestFileToRead "decrypted"

requestFileToReadEncrypted :: IO Handle
requestFileToReadEncrypted = requestFileToRead "encrypted"

requestFileToRead :: String -> IO Handle
requestFileToRead fileType = do
  putStrLn $ "please enter a path for the " ++ fileType ++
             " file to be loaded from" ++
             " for encryption"
  path <- getLine
  fh <- try $ openFile path ReadMode
  case fh of
    Left e -> do
      putStrLn $ "could not open a file handle for reading from " ++
                 show path
      putStrLn $ "because: " ++ show (e :: IOException)
      requestFileToReadDecrypted
    Right fh -> return fh

writeHandleFromUser = openFile "key.txt" WriteMode
readHandleFromUser = openFile "key.txt" ReadMode


  
-- main :: IO ()
-- main = do
--   putStrLn "please enter encrypt or decrypt"
--   str <- getLine
--   let reader = (traverse readA "encrypt" *> pure encrypt) `mplus`
--                (traverse readA "decrypt" *> pure decrypt)
--   maybe
--     (putStrLn ("invalid option " + str) >> main)
--     (const $ putStrLn "good choise")
--     (runReaderT reader str)
--   return ()
