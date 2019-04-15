{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Foreign.Generator (Generatable, generateCode) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad

import Foreign.AST

class Generatable a where
  writeAST :: a -> Generator ()

  generateCode :: a -> String
  generateCode ast = execWriter (runStateT (runGenerator $ writeAST ast) 0)

instance Generatable Program where
  writeAST (Program units) = mapM_ writeAST units

instance Generatable TranslationUnit where
  writeAST (UFunction x) = writeAST x                 >> writeNewLine
  writeAST (UVariable x) = writeAST x >> writeStr ";" >> writeNewLine
  writeAST (UEmpty     ) = skip

instance Generatable VariableDefinition where
  writeAST (DValue t n v) = do
    writeAST t
    writeSpace
    writeStr n
    writeOp "="
    writeAST v

  writeAST (DArray (TPointer t) n vs) = do
    writeAST t
    writeSpace
    writeStr n
    writeInBrackets skip
    writeOp "="
    writeInBraces $ writeCommaSep $ map writeAST vs

instance Generatable FunctionDefinition where
  writeAST (DFunction t n as b) = do
    writeAST t
    writeSpace
    writeStr n
    writeInParens $ writeCommaSep $ for as $ \(at, an) -> do
      writeAST at
      writeSpace
      writeStr an
    writeBlock b
    where for = flip map

instance Generatable Statement where
  writeAST (SIfElse c p n) = do
    writeIndent
    writeStr "if"
    writeSpace
    writeInParens $ writeAST c
    writeBlock p
    when (not . null $ n) $ do
      writeSpace
      writeStr "else"
      writeBlock n

  writeAST (SWhile c b) = do
    writeIndent
    writeStr "while"
    writeSpace
    writeInParens $ writeAST c
    writeBlock b

  writeAST (SReturn x) = writeStmt $ do
    writeStr "return"
    writeSpace
    writeAST x

  writeAST (SEvaluate x) = writeStmt $ writeAST x

  writeAST SContinue = writeStmt $ writeStr "continue"
  writeAST SBreak    = writeStmt $ writeStr "break"

instance Generatable Expression where
  writeAST (ECast t x) = writeInParens $ do
    writeInParens $ writeAST t
    writeSpace
    writeInParens $ writeAST x

  writeAST (ECond c p n) = writeInParens $ do
    writeAST c
    writeOp "?"
    writeAST p
    writeOp ":"
    writeAST n

  writeAST (EBinOp "[]" x y) = do
    writeInParens   $ writeAST x
    writeInBrackets $ writeAST y

  writeAST (EBinOp op x y) = writeInParens $ do
    writeAST x
    writeOp op
    writeAST y

  writeAST (EUnOp op x) = writeInParens $ do
    writeStr op
    writeAST x

  writeAST (ECall x as) = do
    writeStr x
    writeInParens $ writeCommaSep $ map writeAST as

  writeAST (EName   x) = writeStr x
  writeAST (EDefine x) = writeAST x

  writeAST (EBool  True) = writeStr "true"
  writeAST (EBool False) = writeStr "false"
  writeAST (EInt      x) = writeStr $ show x
  writeAST (EFloat    x) = writeStr $ show x
  writeAST (EChar     x) = writeStr $ show x
  writeAST (EStr      x) = writeStr $ show x

  writeAST ENull = skip

instance Generatable TypeIdentifier where
  writeAST (TPointer x) = writeAST x >> writeStr "*"
  writeAST (TInt8     ) = writeStr "int8_t"
  writeAST (TUInt8    ) = writeStr "uint8_t"
  writeAST (TInt16    ) = writeStr "int16_t"
  writeAST (TUInt16   ) = writeStr "uint16_t"
  writeAST (TInt32    ) = writeStr "int32_t"
  writeAST (TUInt32   ) = writeStr "uint32_t"
  writeAST (TFloat    ) = writeStr "float"
  writeAST (TBool     ) = writeStr "bool"
  writeAST (TChar     ) = writeStr "char"
  writeAST (TVoid     ) = writeStr "void"

newtype Generator a = Generator {
    runGenerator :: StateT Int (Writer String) a
  } deriving (Functor, Applicative, Monad, MonadState Int, MonadWriter String)

skip :: Generator ()
skip = return ()

writeStr :: String -> Generator ()
writeStr = tell

writeSpace :: Generator ()
writeSpace = writeStr " "

writeNewLine :: Generator ()
writeNewLine = writeStr "\n"

writeOp :: String -> Generator ()
writeOp op = writeSpace >> writeStr op >> writeSpace

writeInParens :: Generator () -> Generator ()
writeInParens x = writeStr "(" >> x >> writeStr ")"

writeInBraces :: Generator () -> Generator ()
writeInBraces x = writeStr "{" >> x >> writeStr "}"

writeInBrackets :: Generator () -> Generator ()
writeInBrackets x = writeStr "[" >> x >> writeStr "]"

writeCommaSep :: [Generator ()] -> Generator ()
writeCommaSep [    ] = skip
writeCommaSep [x   ] = x
writeCommaSep (x:xs) = x >> writeStr ", " >> writeCommaSep xs

writeIndent :: Generator ()
writeIndent = get >>= (\level -> writeStr $ replicate level '\t')

writeStmt :: Generator () -> Generator ()
writeStmt x = writeIndent >> x >> writeStr ";"

writeBlock :: [Statement] -> Generator ()
writeBlock [   ] = writeSpace >> writeInBraces skip
writeBlock stmts = do
  writeSpace
  writeInBraces $ do
    writeNewLine
    modify succ
    writeBlock' stmts
    modify pred
    writeIndent
  where
    writeBlock' [    ] = skip
    writeBlock' (x:xs) = do
      writeAST x
      writeNewLine
      writeBlock' xs
