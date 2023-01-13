import Control.Applicative
import Control.Exception

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Cont

import Data.Functor.Identity

import System.Random
import System.IO

import Encryptor
import KeyActions
import Encryption
import KeyComposed
import Parse
import IO

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
    (Encryption keyActions encryptor) =
      keyActionMap (mapGenerate fromGen
                    . mapSave fromSave
                    . mapLoad fromLoad)
      $ strongPower 7 (power 3 ((Encryption intActions shiftUp)
                                `strongCompose`
                                (Encryption intActions shiftXor))
                       `compose`
                       ((Encryption intActions shiftXor)
                        `strongCompose`
                        (Encryption intActions shiftUp)))
    encryption = do
      let key = runIdentity . generate $ keyActions
      save keyActions key
      uncont (do dec <- persistantFileRead "decrypted" "encryption"
                 persistantFileWrite "encrypted" . encrypt encryptor key $ dec)
    decryption =
      uncont (do
                 key <- load keyActions
                 enc <- persistantFileRead "encrypted" "decryption"
                 persistantFileWrite "decrypted" . decrypt encryptor key $ enc)
    
uncont :: Applicative m =>  ContT r m r -> m r
uncont = flip runContT pure

intActions :: KeyActions
                (State StdGen)
                String
                (StateT String (Either String))
                Int
intActions =
  KeyActions
    (state random)
    (\k -> show k ++ "\n")
    (many parseWhiteSpace *> parseInt <* many parseWhiteSpace)


fromGen :: State StdGen a -> Identity a
fromGen gen = Identity $ evalState gen (mkStdGen 137)

fromSave :: String -> IO ()
fromSave saving =
  runContT (persistantWriteFileRequest
            (requestString "please enter a key path to write into")
            saving)
    return

fromLoad :: StateT String (Either String) k -> ContT () IO k
fromLoad loading = do
      str <- persistantReadFileRequest
             $ requestString "please enter a key path to read from"
      case runStateT loading str of
        Right (r, "") -> return r
        Right (r, remains) ->
          lift . fail $ "got remains after key parse: " ++ show remains
        Left msg -> do
          lift . putStrLn $ ("error: " ++ msg)
          fromLoad loading

persistantFileRead :: String -> String -> ContT () IO String
persistantFileRead fileType reason =
  persistantReadFileRequest
    . requestString
    $ "please enter a path for the " ++ fileType
    ++ " file to be loaded from" ++ " for " ++ reason

persistantFileWrite :: String -> String -> ContT () IO ()
persistantFileWrite fileType =
  persistantWriteFileRequest
  . requestString
  $ "please enter a path for the " ++ fileType
  ++ " file to be saved at"

requestString :: String -> IO String
requestString request =
  putStrLn request >> getLine
