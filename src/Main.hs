import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import System.Random
import System.IO
import Encryptor
import KeyActions
import Encryption
import KeyComposed

performEncryption :: (Monad m) =>
                     (enc -> m ()) ->
                     (dec -> m ()) ->
                     (m dec) ->
                     (m enc) ->
                     Encryption m m m k dec enc ->
                     (m (), m ())
performEncryption consumeEnc consumeDec getDec getEnc (Encryption ka encryptor) =
  (performEnc, performDec) where
  performEnc = do
    key <- generate ka
    save ka key
    dec <- getDec
    consumeEnc . encrypt encryptor key $ dec
  performDec = do
    key <- load ka
    enc <- getEnc
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
      performEncryption (persistantFileWrite "encrypted")
                        (persistantFileWrite "decrypted")
                        (persistantFileRead "decrypted" "encryption")
                        (persistantFileRead "encrypted" "decryption")
                        (keyActionMap (mapGenerate fromGen .
                                       mapSave fromSave .
                                       mapLoad fromLoad)
                          alg)
    alg = 
      strongPower 7 (power 3 ((Encryption intActions shiftUp)
                              `strongCompose`
                              (Encryption intActions shiftMult))
                     `compose`
                     ((Encryption intActions shiftXor)
                      `strongCompose`
                      (Encryption intActions shiftMult)))


fromGen :: State StdGen a -> IO a
fromGen gen = pure $ evalState gen (mkStdGen 137)

fromSave :: ReaderT Handle IO () -> IO ()
fromSave saving =
  persistantWriteFileRequest
    (requestString "please enter a key path to write into") >>=
  runReaderT saving

fromLoad :: StateT String (Either String) k -> IO k
fromLoad loading = do
  h <- persistantReadFileRequest
         (requestString "please enter a key path to read from")
  str <- hGetContents h
  either failureToIO parsedToIO
    (runStateT loading str)
  where
    parsedToIO (r, "") = return r
    parsedToIO (r, remains) = fail ("got remains after key parse: " ++ show remains)
    failureToIO msg = putStrLn ("error: " ++ msg) >> fromLoad loading

persistantFileRead :: String -> String -> IO String
persistantFileRead fileType reason =
  persistantReadFileRequest
    (requestString $ "please enter a path for the " ++ fileType ++
                     " file to be loaded from" ++
                     " for " ++ reason) >>=
  hGetContents

persistantFileWrite :: String -> String -> IO ()
persistantFileWrite fileType enc = do
  fh <- persistantWriteFileRequest
        (requestString $ "please enter a path for the " ++ fileType ++
                         " file to be saved at")
  hPutStr fh enc

persistantReadFileRequest :: IO String -> IO Handle
persistantReadFileRequest =
  persistantOpenFile
    ReadMode
    (\path -> "could not open a file handle for reading from " ++ show path)

persistantWriteFileRequest :: IO String -> IO Handle
persistantWriteFileRequest =
  persistantOpenFile
    WriteMode
    (\path -> "could not open a file handle for writing into " ++ show path)

persistantOpenFile :: IOMode ->
                      (String -> String) ->
                      IO String ->
                      IO Handle
persistantOpenFile ioMode errorSummaryFromPath requestPath = do
  path <- requestPath
  catch (openFile path ioMode)
    (\e -> do
        putStrLn $ errorSummaryFromPath path
        putStrLn $ "because: " ++ show (e :: IOException)
        persistantOpenFile ioMode errorSummaryFromPath requestPath)
                      

requestString :: String -> IO String
requestString request =
  putStrLn request >> getLine
