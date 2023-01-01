import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import System.Random
import System.IO
import Encryptor
import KeyActions
import Encryption

performEncryption :: (Monad m) =>
                     (enc -> m ()) ->
                     (dec -> m ()) ->
                     (m dec) ->
                     (m enc) ->
                     Encryption m m m k dec enc ->
                     (m (), m ())
performEncryption consumeEnc consumeDec dec enc (Encryption ka encryptor) =
  (performEnc, performDec) where
  performEnc = do
    key <- generate ka
    save ka key
    dec <- dec
    consumeEnc . encrypt encryptor key $ dec
  performDec = do
    key <- load ka
    enc <- enc
    consumeDec . decrypt encryptor key $ enc

main :: IO ()
main = do
  operation <- requestString "please enter operation (encryption|decryption)"
  case operation of
    "encryption" -> encryption
    "decryption" -> decryption
    _ -> do
      putStrLn $ "did not understand operation " ++ show operation
      main
  where
    (encryption,decryption) =
      performEncryption writeEnc writeDec
                        (requestFileToReadDecrypted >>= hGetContents)
                        (requestFileToReadEncrypted >>= hGetContents)
                        (Encryption (mapGenerate fromGen .
                                     mapSave fromSave .
                                     mapLoad fromLoad $
                                     intActions)
                          shiftUp)


fromGen :: State StdGen a -> IO a
fromGen gen = pure $ evalState gen (mkStdGen 137)

fromSave :: ReaderT Handle IO () -> IO ()
fromSave saving = userKeyWriteHandle >>= runReaderT saving

fromLoad :: StateT String (Either String) k -> IO k
fromLoad loading = do
  h <- userKeyReadHandle
  str <- hGetContents h
  either fail parsedToIO
    (runStateT loading str)
  where
    parsedToIO (r, "") = return r
    parsedToIO (r, remains) = fail ("got remains after key parse: " ++ show remains)

writeEnc :: String -> IO ()
writeEnc enc = do
  fh <- requestFileToWriteEncrypted
  hPutStr fh enc

writeDec :: String -> IO ()
writeDec enc = do
  fh <- requestFileToWriteDecrypted
  hPutStr fh enc

requestFileToWriteEncrypted :: IO Handle
requestFileToWriteEncrypted = requestFileToWrite "encrypted"

requestFileToWriteDecrypted :: IO Handle
requestFileToWriteDecrypted = requestFileToWrite "decrypted"

requestFileToReadDecrypted :: IO Handle
requestFileToReadDecrypted = requestFileToRead "decrypted" "encryption"

requestFileToReadEncrypted :: IO Handle
requestFileToReadEncrypted = requestFileToRead "encrypted" "decryption"

userKeyWriteHandle :: IO Handle
userKeyWriteHandle =
  persistantWriteFileRequest
    (requestString "please enter a key path to write into")

userKeyReadHandle :: IO Handle
userKeyReadHandle =
  persistantReadFileRequest
    (requestString "please enter a key path to read from")

requestFileToRead :: String -> String -> IO Handle
requestFileToRead fileType reason =
  persistantReadFileRequest
    (requestString $ "please enter a path for the " ++ fileType ++
                     " file to be loaded from" ++
                     " for " ++ reason)

requestFileToWrite :: String -> IO Handle
requestFileToWrite fileType =
  persistantWriteFileRequest
    (requestString $ "please enter a path for the " ++ fileType ++
                     " file to be saved at")

persistantReadFileRequest :: IO String -> IO Handle
persistantReadFileRequest requestPath = do
  path <- requestPath
  catch (openFile path ReadMode)
    (\e -> do
        putStrLn $ "could not open a file handle for reading from " ++
                   show path
        putStrLn $ "because: " ++ show (e :: IOException)
        requestFileToReadDecrypted)

persistantWriteFileRequest :: IO String -> IO Handle
persistantWriteFileRequest requestPath = do
  path <- requestPath
  catch (openFile path WriteMode)
    (\e -> do
        putStrLn $ "could not open a file handle for writing into " ++
                   show path
        putStrLn $ "because: " ++ show (e :: IOException)
        requestFileToReadDecrypted)

requestString :: String -> IO String
requestString request =
  putStrLn request >> getLine
