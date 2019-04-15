{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Native.Lexer where

import Control.Monad.Except
import Control.Monad.State

import Data.Char
import Data.Word

import Location
}

$digit =  0-9
$alpha = [a-zA-Z]

$idStart  =  $alpha
$idLetter = [$alpha $digit _]

$litChar = [\ -\~]
@escChar = "\\0" | "\\a" | "\\b" | "\\f" | "\\n" | "\\r" | "\\t" | "\\v" | "\\\\" | "\\\'"

@char = @escChar | $litChar

@symbol = "(" | ")" | "[" | "]" | "{" | "}" | "," | ";" | ":" | "="

@operator
  = "+"  | "-"  | "*"  | "/"  | "%"
  | "&"  | "|"  | "^"  | "<<" | ">>" | "~"
  | "&&" | "||" | "!"
  | "<"  | "<=" | ">"  | ">="
  | "==" | "!="

@keyword
  = event
  | invoked
  | handle
  | register
  | when
  | return
  | break
  | continue
  | if
  | then
  | else
  | while
  | extern

@type
  = pin
  | int8
  | uint8
  | int16
  | uint16
  | int32
  | uint32
  | char
  | float
  | bool
  | void

hebi :-
  $white+ ;
  "//".*  ;

  "@" $digit+ .+ { changeLocation }

  @symbol   { mkTok TokSymbol   }
  @operator { mkTok TokOperator }
  @keyword  { mkTok TokKeyword  }
  @type     { mkTok TokType     }

  $digit+            { mkTok $ \s -> TokInt   (read s) }
  $digit+ \. $digit+ { mkTok $ \s -> TokFloat (read s) }

  true  { mkTok $ const $ TokBool True  }
  false { mkTok $ const $ TokBool False }

  D $digit+ { mkTok $ \s -> TokPin False (read . tail $ s) }
  A $digit+ { mkTok $ \s -> TokPin True  (read . tail $ s) }

  "'"  @char  "'"  { mkTok $ \s -> TokChar (read s) }
  \"   @char* \"   { mkTok $ \s -> TokStr  (read s) }

  $idStart $idLetter* (\. $idStart $idLetter*)? { mkTok TokIdentifier }

{
type Byte      = Word8
type AlexInput = (String, Location)

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (   [] ,   _) = Nothing
alexGetByte ((x:xs), loc) = Just (fromIntegral . ord $ x, (xs, loc')) where
  loc' = move loc x
  move (At f (l, c)) x
    | x == '\n' = At f (l + 1, 1)
    | otherwise = At f (l, c + 1)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

newtype Parser a = Parser {
    unParser :: ExceptT String (State ParserState) a
  } deriving (Functor, Applicative, Monad, MonadError String, MonadState ParserState)

data ParserState = ParserState {
    pstRest     :: String,
    pstLocation :: Location
  }

runParser :: Parser a -> String -> String -> Either String a
runParser x src file = evalState (runExceptT $ unParser x) initParserState where
  initParserState = ParserState {
      pstRest     = src,
      pstLocation = At file (1, 1)
    }

getInput :: Parser AlexInput
getInput = (,) <$> (gets pstRest) <*> (gets pstLocation)

setInput :: AlexInput -> Parser ()
setInput (rest, loc) = modify $ \pst -> pst { pstRest = rest, pstLocation = loc }

getLocation :: Parser Location
getLocation = gets pstLocation

setLocation :: Location -> Parser ()
setLocation loc = modify $ \pst -> pst { pstLocation = loc }

lexerMonadScan :: Parser (Located Token)
lexerMonadScan = do
  inp <- getInput
  case alexScan inp 0 of
    AlexEOF                -> getLocation >>= (\loc -> return $ (Located loc) TokEOF)
    AlexSkip  inp' len     -> setInput inp' >> lexerMonadScan
    AlexToken inp' len tok -> setInput inp' >> tok inp len
    AlexError (_, loc)     -> throwError $ show loc ++ ": lexical error"

data Token
  = TokSymbol      String
  | TokKeyword     String
  | TokOperator    String
  | TokType        String
  | TokIdentifier  String
  | TokInt         Int
  | TokFloat       Float
  | TokBool        Bool
  | TokPin         Bool Int
  | TokChar        Char
  | TokStr         String
  | TokEOF
  deriving (Show)

mkTok :: (String -> Token) -> AlexInput -> Int -> Parser (Located Token)
mkTok tok (s, loc) len = return . (Located loc) . tok . (take len) $ s

changeLocation :: AlexInput -> Int -> Parser (Located Token)
changeLocation (s, loc) len = (setLocation $ (uncurry At) $ f s) >> lexerMonadScan where
  f = (\(l, f) -> (trim f, (read l, 1))) . (break (== ' ')) . trim . (drop 1) . (take len)

  trim = dropWhile (== ' ')
}
