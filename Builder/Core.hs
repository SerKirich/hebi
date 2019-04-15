{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Builder.Core (
    Builder,
    BuilderConfig(..),
    build,
    getHebiPath,
    getBoard
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import System.IO

import Board

data BuilderConfig = BuilderConfig {
    bcfgHebiPath :: FilePath,
    bcfgBoard    :: Board
  }

newtype Builder a = Builder {
    runBuilder :: ExceptT String (ReaderT BuilderConfig IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError String, MonadReader BuilderConfig)

build :: BuilderConfig -> Builder () -> IO ()
build bcfg b = do
  res <- runReaderT (runExceptT $ runBuilder b) bcfg
  case res of
    Left err -> hPutStrLn stderr err
    Right () -> return ()

getHebiPath :: Builder FilePath
getHebiPath = asks bcfgHebiPath

getBoard :: Builder Board
getBoard = asks bcfgBoard
