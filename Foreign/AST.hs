module Foreign.AST where

newtype Program = Program [TranslationUnit]

data TranslationUnit
  = UFunction FunctionDefinition
  | UVariable VariableDefinition
  | UEmpty

data VariableDefinition
  = DValue TypeIdentifier Identifier  Expression
  | DArray TypeIdentifier Identifier [Expression]

data FunctionDefinition
  = DFunction TypeIdentifier Identifier [(TypeIdentifier, Identifier)] [Statement]

data Statement
  = SIfElse   Expression [Statement] [Statement]
  | SWhile    Expression [Statement]
  | SReturn   Expression
  | SEvaluate Expression
  | SContinue
  | SBreak

data Expression
  = EDefine VariableDefinition
  | ECast   TypeIdentifier          Expression
  | ECond   Expression  Expression  Expression
  | EBinOp  Identifier  Expression  Expression
  | EUnOp   Identifier  Expression
  | ECall   Identifier [Expression]
  | EName   Identifier
  | EInt    Int
  | EFloat  Float
  | EBool   Bool
  | EChar   Char
  | EStr    String
  | ENull

data TypeIdentifier
  = TPointer TypeIdentifier
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

type Identifier = String

mapStmt :: (Statement -> Statement) -> Statement -> Statement
mapStmt f (SIfElse c p n) = f $ SIfElse c (map (mapStmt f) p) (map (mapStmt f) n)
mapStmt f (SWhile  c   b) = f $ SWhile  c (map (mapStmt f) b)
mapStmt f x = f x

mapExpr :: (Expression -> Expression) -> Expression -> Expression
mapExpr f (ECast     t x) = f $ ECast  t            (mapExpr f x)
mapExpr f (ECond   c p n) = f $ ECond (mapExpr f c) (mapExpr f p) (mapExpr f n)
mapExpr f (EBinOp op l r) = f $ EBinOp op           (mapExpr f l) (mapExpr f r)
mapExpr f (EUnOp  op   x) = f $ EUnOp  op           (mapExpr f x)
mapExpr f (ECall name as) = f $ ECall name (map (mapExpr f) as)
mapExpr f x = f x
