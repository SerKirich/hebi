module Builder.Preprocessor (preprocess) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad

import System.Directory
import System.FilePath
import System.IO

import Builder.Core

preprocess :: FilePath -> Builder String
preprocess hebiFile = do
  source <- liftIO $ readFile hebiFile
  lines' <- forM (zip (lines source) [1..]) $ \(line, n) -> do
    if (not . null $ line) && (head line == '#')
    then do
      let command = takeWhile (/= ' ') $ tail line
      case command of
        "include" -> do
          includeFile <- getIncludeFile $ (drop 1 $ dropWhile (/= ' ') $ line) <.> "hebi"
          (\inc -> unlines [tag includeFile 1, inc, tag hebiFile n]) <$> (liftIO $ readFile includeFile)
        c -> throwError $ "Unknown preprocessor directive encountered: #" ++ c
    else return line
  return $ unlines lines'

getIncludeFile :: String -> Builder FilePath
getIncludeFile includeFile = do
  locals <- liftIO $ getCurrentDirectory >>= getDirectoryContents
  if includeFile `elem` locals
  then return includeFile
  else do
    hebiPath <- getHebiPath
    globals  <- liftIO $ getDirectoryContents hebiPath
    if includeFile `elem` globals
    then return $ hebiPath </> includeFile
    else throwError $ "Couldn't find " ++ includeFile

tag :: FilePath -> Int -> String
tag f l = "@" ++ (show $ l - 1) ++ " " ++ f
