module CLI (
    HebiCommand(..),
    UploadOptions(..),
    CompileOptions(..),
    hebiCommand
  ) where

import Options.Applicative

import Data.Semigroup ((<>))

import Builder
import Board

data HebiCommand
  = Upload  UploadOptions
  | Compile CompileOptions
  deriving (Show)

data UploadOptions
  = UploadOptions {
    uploadSketch :: FilePath,
    uploadBoard  :: Board,
    uploadPort   :: FilePath
  } deriving (Show)

data CompileOptions
  = CompileOptions {
    compileSketch :: FilePath,
    compileBoard  :: Board
  } deriving (Show)

hebiCommand :: Parser HebiCommand
hebiCommand = subparser (
    (command "upload"  $ info uploadCommand  $ progDesc "Compile and upload sketch to the specified board") <>
    (command "compile" $ info compileCommand $ progDesc "Just compile sketch for specified board")
  )

uploadCommand :: Parser HebiCommand
uploadCommand = Upload <$> uploadOptions

compileCommand :: Parser HebiCommand
compileCommand = Compile <$> compileOptions

uploadOptions :: Parser UploadOptions
uploadOptions = UploadOptions <$> sketchArgument <*> boardOption <*> portOption

compileOptions :: Parser CompileOptions
compileOptions = CompileOptions <$> sketchArgument <*> boardOption

sketchArgument :: Parser FilePath
sketchArgument = argument str $ metavar "SKETCH"

boardOption :: Parser Board
boardOption = option boardReader $ long "board" <> short 'b' <> metavar "BOARD" <> value Uno <> help "Board to build for"

portOption :: Parser FilePath
portOption = option str $ long "port" <> short 'p' <> metavar "PORT" <> help "Serial port to upload via"

boardReader :: ReadM Board
boardReader = maybeReader $ \s -> case s of
  "uno" -> Just Uno
  _     -> Nothing
