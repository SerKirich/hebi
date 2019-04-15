{
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}

module Native.Parser (parseHebi) where

import Control.Monad.Except

import Location

import Native.Lexer
import Native.AST
}

%name parse

%error     { parseError    }
%lexer     { lexer         } { Located _ TokEOF }
%monad     { Parser        }
%tokentype { Located Token }

%token
  '(' { Located _ (TokSymbol "(") }
  ')' { Located _ (TokSymbol ")") }
  '[' { Located _ (TokSymbol "[") }
  ']' { Located _ (TokSymbol "]") }
  '{' { Located _ (TokSymbol "{") }
  '}' { Located _ (TokSymbol "}") }
  ',' { Located _ (TokSymbol ",") }
  ';' { Located _ (TokSymbol ";") }
  ':' { Located _ (TokSymbol ":") }
  '=' { Located _ (TokSymbol "=") }

  '+'  { Located _ (TokOperator  "+") }
  '-'  { Located _ (TokOperator  "-") }
  '*'  { Located _ (TokOperator  "*") }
  '/'  { Located _ (TokOperator  "/") }
  '%'  { Located _ (TokOperator  "%") }
  '&'  { Located _ (TokOperator  "&") }
  '|'  { Located _ (TokOperator  "|") }
  '^'  { Located _ (TokOperator  "^") }
  '<<' { Located _ (TokOperator "<<") }
  '>>' { Located _ (TokOperator ">>") }
  '~'  { Located _ (TokOperator  "~") }
  '&&' { Located _ (TokOperator "&&") }
  '||' { Located _ (TokOperator "||") }
  '!'  { Located _ (TokOperator  "!") }
  '<'  { Located _ (TokOperator  "<") }
  '<=' { Located _ (TokOperator "<=") }
  '>'  { Located _ (TokOperator  ">") }
  '>=' { Located _ (TokOperator ">=") }
  '==' { Located _ (TokOperator "==") }
  '!=' { Located _ (TokOperator "!=") }

  event    { Located _ (TokKeyword    "event") }
  invoked  { Located _ (TokKeyword  "invoked") }
  handle   { Located _ (TokKeyword   "handle") }
  register { Located _ (TokKeyword "register") }
  when     { Located _ (TokKeyword     "when") }
  return   { Located _ (TokKeyword   "return") }
  break    { Located _ (TokKeyword    "break") }
  continue { Located _ (TokKeyword "continue") }
  if       { Located _ (TokKeyword       "if") }
  then     { Located _ (TokKeyword     "then") }
  else     { Located _ (TokKeyword     "else") }
  while    { Located _ (TokKeyword    "while") }
  extern   { Located _ (TokKeyword   "extern") }

  pin    { Located _ (TokType    "pin") }
  int8   { Located _ (TokType   "int8") }
  uint8  { Located _ (TokType  "uint8") }
  int16  { Located _ (TokType  "int16") }
  uint16 { Located _ (TokType "uint16") }
  int32  { Located _ (TokType  "int32") }
  uint32 { Located _ (TokType "uint32") }
  float  { Located _ (TokType  "float") }
  bool   { Located _ (TokType   "bool") }
  char   { Located _ (TokType   "char") }
  void   { Located _ (TokType   "void") }

  int_literal   { Located _ (TokInt   _) }
  float_literal { Located _ (TokFloat _) }
  bool_literal  { Located _ (TokBool  _) }
  pin_literal   { Located _ (TokPin _ _) }
  char_literal  { Located _ (TokChar  _) }
  str_literal   { Located _ (TokStr   _) }

  identifier { Located _ (TokIdentifier _) }

%nonassoc ELSE
%nonassoc IF
%nonassoc '='

%nonassoc else
%left     '||'
%left     '&&'
%left     '|'
%left     '^'
%left     '&'
%nonassoc '==' '!='
%nonassoc '<'  '<='  '>'  '>='
%left     '<<' '>>'
%left     '+'  '-'
%left     '*'  '/'  '%'
%right    REF DER NEG  '~'  '!'
%left     '['

%%

program :: { Program }
  : extern_declarations translation_units event_registrations { Program $1 $2 $3 }

translation_units :: { [L TranslationUnit] }
  : {- empty -}                        {      [] }
  | translation_unit translation_units { $1 : $2 }

translation_unit :: { L TranslationUnit }
  : event_definition    { at $1 $ UEvent    (un $1) }
  | function_definition { at $1 $ UFunction (un $1) }
  | variable_definition { at $1 $ UVariable (un $1) }

extern_declarations :: { [L ExternDeclaration] }
  : {- empty -}                            {      [] }
  | extern_declaration extern_declarations { $1 : $2 }

extern_declaration :: { L ExternDeclaration }
  : extern type_identifier identifier '(' type_identifiers ')' ';' { at $1 $ DExternFunction (un $2) (ux $3) $5 }
  | extern type_identifier identifier                          ';' { at $1 $ DExternVariable (un $2) (ux $3)    }

event_registrations :: { [L EventRegistration] }
  : {- empty -}                            {      [] }
  | event_registration event_registrations { $1 : $2 }

event_registration :: { L EventRegistration }
  : register identifier '(' expressions ')' ';' { at $1 $ REvent (ux $2) $4 }

event_definition :: { L EventDefinition }
  : event identifier '(' arguments ')' event_body { at $1 $ $6 (ux $2) $4 }

event_body :: { EventBody }
  : '{' variable_definitions invoked block handle block '}' { eventBody $2 $4 $6 }

function_definition :: { L FunctionDefinition }
  : type_identifier identifier '(' arguments ')' block { at $1 $ DFunction (un $1) (ux $2) $4 $6 }

variable_definitions :: { [L VariableDefinition] }
  : {- empty -}                              {      [] }
  | variable_definition variable_definitions { $1 : $2 }

variable_definition :: { L VariableDefinition }
  : type_identifier identifier '='     expression      ';' { at $1 $ DValue (un $1) (ux $2) $4 }
  | type_identifier identifier '=' '{' expressions '}' ';' { at $1 $ DArray (un $1) (ux $2) $5 }

arguments :: { [(TypeIdentifier, Identifier)] }
  : {- empty -}                              {                  [] }
  | type_identifier identifier               { (un $1, ux $2) : [] }
  | type_identifier identifier ',' arguments { (un $1, ux $2) : $4 }

block :: { [L Statement] }
  : '{' statements '}' {  $2  }
  | ':' statement      { [$2] }

statements :: { [L Statement] }
  : {- empty -}          {      [] }
  | statement statements { $1 : $2 }

statement :: { L Statement }
  : variable_definition                       { at $1 $ SDefine     $1       }
  | identifier '=' expression ';'             { at $1 $ SAssign (ux $1) $3   }
  | if expression block            %prec IF   { at $1 $ SIfElse     $2 $3 [] }
  | if expression block else block %prec ELSE { at $1 $ SIfElse     $2 $3 $5 }
  | while expression block                    { at $1 $ SWhile      $2 $3    }
  | return expression ';'                     { at $1 $ SReturn     $2       }
  | when expression ';'                       { at $1 $ SWhen       $2       }
  | expression ';'                            { at $1 $ SEvaluate   $1       }
  | continue ';'                              { at $1 $ SContinue            }
  | break ';'                                 { at $1 $ SBreak               }

expressions :: { [L Expression] }
  : {- empty -}                {      [] }
  | expression                 { $1 : [] }
  | expression ',' expressions { $1 : $3 }

expression :: { L Expression }
  : type_identifier '(' expression ')'            { at $1 $ ECast   (un $1) $3     }
  | if expression then expression else expression { at $1 $ ECond       $2  $4  $6 }
  | expression '+'  expression                    { at $1 $ EBinOp "+"  $1  $3     }
  | expression '-'  expression                    { at $1 $ EBinOp "-"  $1  $3     }
  | expression '*'  expression                    { at $1 $ EBinOp "*"  $1  $3     }
  | expression '/'  expression                    { at $1 $ EBinOp "/"  $1  $3     }
  | expression '%'  expression                    { at $1 $ EBinOp "%"  $1  $3     }
  | expression '&'  expression                    { at $1 $ EBinOp "&"  $1  $3     }
  | expression '|'  expression                    { at $1 $ EBinOp "|"  $1  $3     }
  | expression '^'  expression                    { at $1 $ EBinOp "^"  $1  $3     }
  | expression '<<' expression                    { at $1 $ EBinOp "<<" $1  $3     }
  | expression '>>' expression                    { at $1 $ EBinOp ">>" $1  $3     }
  | expression '&&' expression                    { at $1 $ EBinOp "&&" $1  $3     }
  | expression '||' expression                    { at $1 $ EBinOp "||" $1  $3     }
  | expression '<'  expression                    { at $1 $ EBinOp "<"  $1  $3     }
  | expression '<=' expression                    { at $1 $ EBinOp "<=" $1  $3     }
  | expression '>'  expression                    { at $1 $ EBinOp ">"  $1  $3     }
  | expression '>=' expression                    { at $1 $ EBinOp ">=" $1  $3     }
  | expression '==' expression                    { at $1 $ EBinOp "==" $1  $3     }
  | expression '!=' expression                    { at $1 $ EBinOp "!=" $1  $3     }
  | expression '['  expression  ']'               { at $1 $ EBinOp "[]" $1  $3     }
  | '&' expression %prec REF                      { at $1 $ EUnOp  "&"  $2         }
  | '*' expression %prec DER                      { at $1 $ EUnOp  "*"  $2         }
  | '-' expression %prec NEG                      { at $1 $ EUnOp  "-"  $2         }
  | '~' expression                                { at $1 $ EUnOp  "~"  $2         }
  | '!' expression                                { at $1 $ EUnOp  "!"  $2         }
  | identifier '(' expressions ')'                { at $1 $ ECall   (ux $1) $3     }
  | identifier                                    { at $1 $ EName   (ux $1)        }
  | int_literal                                   { at $1 $ EInt    (ux $1)        }
  | float_literal                                 { at $1 $ EFloat  (ux $1)        }
  | bool_literal                                  { at $1 $ EBool   (ux $1)        }
  | pin_literal                                   { at $1 $ ePin    (ux $1)        }
  | char_literal                                  { at $1 $ EChar   (ux $1)        }
  | str_literal                                   { at $1 $ EStr    (ux $1)        }
  | '(' expression ')'                            { at $1 $         (un $2)        }

type_identifiers :: { [TypeIdentifier] }
  : {- empty -}                          {           [] }
  | type_identifier                      { (un $1) : [] }
  | type_identifier ',' type_identifiers { (un $1) : $3 }

type_identifier :: { L TypeIdentifier }
  : type_identifier '*' { at $1 $ TPointer (un $1) }
  | int8                { at $1 $ TInt8            }
  | uint8               { at $1 $ TUInt8           }
  | int16               { at $1 $ TInt16           }
  | uint16              { at $1 $ TUInt16          }
  | int32               { at $1 $ TInt32           }
  | uint32              { at $1 $ TUInt32          }
  | float               { at $1 $ TFloat           }
  | bool                { at $1 $ TBool            }
  | pin                 { at $1 $ TPin             }
  | char                { at $1 $ TChar            }
  | void                { at $1 $ TVoid            }

{
parseHebi :: String -> String -> Either String Program
parseHebi = runParser parse

lexer = (lexerMonadScan >>=)

parseError :: Located Token -> Parser a
parseError _ = do
  (_, loc) <- getInput
  throwError $ show loc ++ ": parse error"

at :: Located b -> a -> Located a
at (Located loc _) x = Located loc x

un :: Located a -> a
un = fromLocated

class Extractable a where
  ux :: Located Token -> a
  ux = un . xt

  xt :: Located Token -> Located a
  xt = fmap extract

  extract :: Token -> a

instance Extractable String where
  extract (TokIdentifier x) = x
  extract (TokStr        x) = x

instance Extractable Char where
  extract (TokChar x) = x

instance Extractable Int where
  extract (TokInt x) = x

instance Extractable Float where
  extract (TokFloat x) = x

instance Extractable Bool where
  extract (TokBool x) = x

instance Extractable (Bool, Int) where
  extract (TokPin x y) = (x, y)

type EventBody = Identifier -> [(TypeIdentifier, Identifier)] -> EventDefinition

eventBody :: [L VariableDefinition] -> [L Statement] -> [L Statement] -> EventBody
eventBody fs i h n as = DEvent n as fs i h

ePin = uncurry EPin
}
