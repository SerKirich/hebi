module Native.AST where

import Location

type L a = Located a

data Program
  = Program [L ExternDeclaration] [L TranslationUnit] [L EventRegistration]
  deriving (Show)

data TranslationUnit
  = UEvent    EventDefinition
  | UFunction FunctionDefinition
  | UVariable VariableDefinition
  deriving (Show)

data ExternDeclaration
  = DExternFunction TypeIdentifier Identifier [TypeIdentifier]
  | DExternVariable TypeIdentifier Identifier
  deriving (Show)

data EventRegistration
  = REvent Identifier [L Expression]
  deriving (Show)

data EventDefinition
  = DEvent Identifier [(TypeIdentifier, Identifier)] [L VariableDefinition] [L Statement] [L Statement]
  deriving (Show)

data FunctionDefinition
  = DFunction TypeIdentifier Identifier [(TypeIdentifier, Identifier)] [L Statement]
  deriving (Show)

data VariableDefinition
  = DValue TypeIdentifier Identifier (L Expression)
  | DArray TypeIdentifier Identifier [L Expression]
  deriving (Show)

data Statement
  = SDefine   (L VariableDefinition)
  | SAssign      Identifier  (L Expression)
  | SIfElse   (L Expression) [L Statement] [L Statement]
  | SWhile    (L Expression) [L Statement]
  | SReturn   (L Expression)
  | SWhen     (L Expression)
  | SEvaluate (L Expression)
  | SContinue
  | SBreak
  deriving (Show)

data Expression
  = ECast  TypeIdentifier (L Expression)
  | ECond  (L Expression) (L Expression) (L Expression)
  | EBinOp    Identifier  (L Expression) (L Expression)
  | EUnOp     Identifier  (L Expression)
  | ECall     Identifier  [L Expression]
  | EName     Identifier
  | EInt   Int
  | EFloat Float
  | EBool  Bool
  | EPin   Bool Int -- isAnalog :: Bool
  | EChar  Char
  | EStr   String
  deriving (Show)

data TypeIdentifier
  = TPointer TypeIdentifier
  | TPin
  | TInt8
  | TUInt8
  | TInt16
  | TUInt16
  | TInt32
  | TUInt32
  | TFloat
  | TBool
  | TChar
  | TVoid
  deriving (Show)

type Identifier = String

mapStmtL :: (Statement -> Statement) -> L Statement -> L Statement
mapStmtL f x = (mapStmt f) `fmap` x

mapStmt :: (Statement -> Statement) -> Statement -> Statement
mapStmt f (SIfElse c p n) = f $ SIfElse c (map (mapStmtL f) p) (map (mapStmtL f) n)
mapStmt f (SWhile  c   b) = f $ SWhile  c (map (mapStmtL f) b)
mapStmt f x = f x
