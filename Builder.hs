module Builder (Builder, BuilderConfig(..), build, getIno, getHex, uploadHex) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad

import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO

import Native.Parser
import Foreign.Generator
import Translator

import Board

import Builder.Core
import Builder.Preprocessor

getIno :: FilePath -> Builder FilePath
getIno hebiFile = do
  hebiPath <- getHebiPath
  source   <- preprocess hebiFile
  case parseHebi source hebiFile of
    Left  err  -> throwError err
    Right nAST -> do
      case translate nAST of
        Left  err  -> throwError $ show err
        Right fAST -> do
          let inoFile = hebiFile -<.> "ino"
          liftIO $ writeFile inoFile $ generateCode fAST
          return inoFile

getHex :: FilePath -> Builder FilePath
getHex inoFile = do
  hebiPath <- getHebiPath
  board    <- getBoard
  liftIO $ copyFile (hebiPath </> "Buildfile") "Makefile"
  (_, _, Just hErr, p) <- liftIO $ createProcess $ buildProc inoFile board
  code <- liftIO $ waitForProcess p
  case code of
    ExitSuccess   -> return $ inoFile <.> "hex"
    ExitFailure _ -> do
      err <- liftIO $ hGetContents hErr
      throwError err

uploadHex :: FilePath -> FilePath -> Builder ()
uploadHex port hexFile = do
  board <- getBoard
  (_, _, Just hErr, p) <- liftIO $ createProcess $ uploadProc hexFile board port
  code <- liftIO $ waitForProcess p
  case code of
    ExitSuccess -> do
      (_, _, _, p) <- liftIO $ createProcess cleanProc
      code <- liftIO $ waitForProcess p
      case code of
        ExitSuccess   -> liftIO $ removeFile "Makefile" -- These lines look strange, but they
        ExitFailure _ -> liftIO $ removeFile "Makefile" -- guarantee termination of the process.
    ExitFailure _ -> do
      err <- liftIO $ hGetContents hErr
      throwError err

buildProc :: FilePath -> Board -> CreateProcess
buildProc sketch board
  = (proc "make" ["SKETCH=" ++ sketch, "BOARD=" ++ show board]) {
      std_out = CreatePipe,
      std_err = CreatePipe
    }

uploadProc :: FilePath -> Board -> FilePath -> CreateProcess
uploadProc hex board port
  = (proc "make" ["upload", "HEX=" ++ hex, "BOARD=" ++ show board, "PORT=" ++ port]) {
      std_out = CreatePipe,
      std_err = CreatePipe
    }

cleanProc :: CreateProcess
cleanProc = (proc "make" ["clean"]) { std_out = CreatePipe, std_err = CreatePipe }
