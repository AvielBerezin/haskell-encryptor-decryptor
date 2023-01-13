module IO
  ( persistantReadFileRequest
  , persistantWriteFileRequest
  ) where

import System.IO
import Control.Exception
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class


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

persistantReadFileRequest :: IO String -> ContT () IO String
persistantReadFileRequest request = do
  handle <- lift $ persistantOpenFile ReadMode openError request
  content <- lift . hGetContents $ handle
  ContT (\withContent -> withContent content >> hClose handle)
  where
    openError path = "could not open a file handle for reading from "
                     ++ show path

persistantWriteFileRequest :: IO String -> String -> ContT () IO ()
persistantWriteFileRequest request content = do
  handle <- lift $ persistantOpenFile WriteMode openError request
  lift $ hPutStr handle content
  lift $ hClose handle
  where
    openError path = "could not open a file handle for writing into "
                      ++ show path

