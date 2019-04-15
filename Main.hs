module Main where

import System.FilePath

import Options.Applicative

import Data.Semigroup ((<>))

import Control.Monad

import Builder
import CLI

main :: IO ()
main = run =<< execParser opts where
  opts = info (helper <*> hebiCommand) (
      fullDesc <>
      progDesc "Compile and upload Hebi sketches" <>
      header "Hebi - event-driven Arduino MCU's programming language"
    )

run :: HebiCommand -> IO ()
run cmd = do
  let hebiPath = "/usr/share/hebi"
  case cmd of
    Compile (CompileOptions sketch board     ) -> build (BuilderConfig hebiPath board) $ void $ compile      sketch
    Upload  (UploadOptions  sketch board port) -> build (BuilderConfig hebiPath board) $        upload  port sketch

compile :: FilePath -> Builder FilePath
compile sketch = getIno sketch >>= getHex

upload :: FilePath -> FilePath -> Builder ()
upload port sketch = compile sketch >>= uploadHex port
