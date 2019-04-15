module Translator.TypeChecker (getType, checkType, requireType) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad

import qualified Data.Map as Map

import Location

import Translator.Core
import Translator.Types
import Translator.TypeSystem

import Native.AST

getType :: Expression -> Translator Type

getType (ECast t x) = return $ toType t

getType (ECond c p n) = do
  requireType Bool (fromLocated c) throwConditionTypeError
  pt <- getType $ fromLocated p
  nt <- getType $ fromLocated n
  if pt == nt
  then return pt
  else do
    throwTypeError "both branches of a conditional expression must have the same type"

getType (EBinOp op l r) = do
  lt <- getType $ fromLocated l
  rt <- getType $ fromLocated r
  case op of
    "+"  -> getTypeOfArithmeticBinOp  op lt rt
    "-"  -> getTypeOfArithmeticBinOp  op lt rt
    "*"  -> getTypeOfArithmeticBinOp  op lt rt
    "/"  -> getTypeOfArithmeticBinOp  op lt rt
    "%"  -> getTypeOfArithmeticBinOp  op lt rt
    "<<" -> getTypeOfBitwiseBinOp     op lt rt
    ">>" -> getTypeOfBitwiseBinOp     op lt rt
    "&"  -> getTypeOfBitwiseBinOp     op lt rt
    "|"  -> getTypeOfBitwiseBinOp     op lt rt
    "^"  -> getTypeOfBitwiseBinOp     op lt rt
    "&&" -> getTypeOfLogicalBinOp     op lt rt
    "||" -> getTypeOfLogicalBinOp     op lt rt
    "<"  -> getTypeOfRelationalBinOp  op lt rt
    "<=" -> getTypeOfRelationalBinOp  op lt rt
    ">"  -> getTypeOfRelationalBinOp  op lt rt
    ">=" -> getTypeOfRelationalBinOp  op lt rt
    "==" -> getTypeOfEquationalBinOp  op lt rt
    "!=" -> getTypeOfEquationalBinOp  op lt rt
    "[]" -> getTypeOfSubscriptOp         lt rt

getType (EUnOp op x) = do
  xt <- getType $ fromLocated x
  case op of
    "+" -> getTypeOfArithmeticUnOp op xt
    "-" -> getTypeOfArithmeticUnOp op xt
    "~" -> getTypeOfBitwiseUnOp    op xt
    "!" -> getTypeOfLogicalUnOp    op xt
    "&" -> getTypeOfPointerUnOp    op xt
    "*" -> getTypeOfPointerUnOp    op xt

getType (ECall name as) = expectFunction name $ \rt eats -> do
  aats <- mapM (getType . fromLocated) as
  if aats /= eats
  then throwArgumentsError name eats aats
  else return rt

getType (EName name) = expectVariable name return

getType (EInt   _) = return anyIntegral
getType (EFloat _) = return Float
getType (EBool  _) = return Bool
getType (EPin _ _) = return Pin
getType (EChar  _) = return Char

getType (EStr   _) = return $ Pointer Char

checkType :: Expression -> Translator ()
checkType = void . getType

requireType :: Type -> Expression -> (Type -> Type -> Translator ()) -> Translator ()
requireType t x err = do
  xt <- getType x
  when (xt /= t) $ err xt t

getTypeOfArithmeticBinOp :: Identifier -> Type -> Type -> Translator Type
getTypeOfArithmeticBinOp op lt rt
  | (lt == anyNumerical) && (rt == anyNumerical) = return anyNumerical
  | otherwise = throwOperandsError op lt rt

getTypeOfBitwiseBinOp :: Identifier -> Type -> Type -> Translator Type
getTypeOfBitwiseBinOp op lt rt
  | (lt == anyIntegral) && (rt == anyIntegral) = return anyIntegral
  | otherwise = throwOperandsError op lt rt

getTypeOfLogicalBinOp :: Identifier -> Type -> Type -> Translator Type
getTypeOfLogicalBinOp _ Bool Bool = return Bool
getTypeOfLogicalBinOp op lt rt = throwOperandsError op lt rt

getTypeOfRelationalBinOp :: Identifier -> Type -> Type -> Translator Type
getTypeOfRelationalBinOp op lt rt
  | (lt == anyNumerical) && (rt == anyNumerical) = return Bool
  | (lt ==         Char) && (rt ==         Char) = return Bool
  | otherwise = throwOperandsError op lt rt

getTypeOfEquationalBinOp :: Identifier -> Type -> Type -> Translator Type
getTypeOfEquationalBinOp op lt rt
  | (lt == anyNumerical) && (rt == anyNumerical) = return Bool
  | (lt ==         Bool) && (rt ==         Bool) = return Bool
  | (lt ==         Char) && (rt ==         Char) = return Bool
  | otherwise = throwOperandsError op lt rt

getTypeOfSubscriptOp :: Type -> Type -> Translator Type
getTypeOfSubscriptOp pt@(Pointer vt) it
  | it == anyIntegral = return vt
  | otherwise = throwOperandsError "[]" pt it

getTypeOfArithmeticUnOp :: Identifier -> Type -> Translator Type
getTypeOfArithmeticUnOp op xt
  | xt == anyNumerical = return anyNumerical
  | otherwise = throwOperandError op xt

getTypeOfBitwiseUnOp :: Identifier -> Type -> Translator Type
getTypeOfBitwiseUnOp op xt
  | xt == anyIntegral = return anyIntegral
  | otherwise = throwOperandError op xt

getTypeOfLogicalUnOp :: Identifier -> Type -> Translator Type
getTypeOfLogicalUnOp _ Bool = return Bool
getTypeOfLogicalUnOp op xt = throwOperandError op xt

getTypeOfPointerUnOp :: Identifier -> Type -> Translator Type
getTypeOfPointerUnOp "&"          xt  = return $ Pointer xt
getTypeOfPointerUnOp "*" (Pointer xt) = return xt
getTypeOfPointerUnOp op xt = throwOperandError op xt
