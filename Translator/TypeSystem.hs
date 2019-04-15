module Translator.TypeSystem where

import Native.AST

data Type
  = Class String [Type]
  | Pointer       Type
  | Pin
  | Int8
  | UInt8
  | Int16
  | UInt16
  | Int32
  | UInt32
  | Char
  | Float
  | Bool
  | Void

instance Eq Type where
  t == (Class _ ts) = t `elem` ts
  (Class _ ts) == t = t `elem` ts

  Pointer t1 == Pointer t2 = t1 == t2

  Pin    == Pin    = True
  Int8   == Int8   = True
  UInt8  == UInt8  = True
  Int16  == Int16  = True
  UInt16 == UInt16 = True
  Int32  == Int32  = True
  UInt32 == UInt32 = True
  Float  == Float  = True
  Bool   == Bool   = True
  Char   == Char   = True
  Void   == Void   = True

  _ == _ = False

instance Show Type where
  show (Class n _) = n

  show (Pointer t) = show t ++ "*"

  show Pin    = "pin"
  show Int8   = "int8"
  show UInt8  = "uint8"
  show Int16  = "int16"
  show UInt16 = "uint16"
  show Int32  = "int32"
  show UInt32 = "uint32"
  show Float  = "float"
  show Bool   = "bool"
  show Char   = "char"
  show Void   = "void"

anyIntegral  = Class "integer" [Int8, UInt8, Int16, UInt16, Int32, UInt32]
anyNumerical = Class "number"  [Int8, UInt8, Int16, UInt16, Int32, UInt32, Float]

toType :: TypeIdentifier -> Type
toType (TPointer t) = Pointer $ toType t
toType TPin    = Pin
toType TInt8   = Int8
toType TUInt8  = UInt8
toType TInt16  = Int16
toType TUInt16 = UInt16
toType TInt32  = Int32
toType TUInt32 = UInt32
toType TFloat  = Float
toType TBool   = Bool
toType TChar   = Char
toType TVoid   = Void
