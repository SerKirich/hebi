{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Translator.Core where

import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as Map
import           Data.Map   (Map)

import Data.List (intercalate, nub)

import Location

import Native.AST

import Translator.Types
import Translator.TypeSystem

data TranslatorFlag
  = FlagLoop
  | FlagIvkd
  deriving (Eq)

data TranslatorState = TranslatorState {
    tstScope          :: Scope,
    tstLocation       :: Location,
    tstFlags          :: [TranslatorFlag],
    tstEventInstances :: EventInstances,
    tstFunctionName   :: Maybe Identifier
  }

initTranslatorState = TranslatorState {
    tstScope          = Map.empty,
    tstLocation       = Somewhere,
    tstFlags          = [],
    tstEventInstances = Map.empty,
    tstFunctionName   = Nothing
  }

newtype Translator a = Translator {
    runTranslator :: StateT TranslatorState (Except Error) a
  } deriving (Functor, Applicative, Monad, MonadError Error, MonadState TranslatorState)

getScope :: Translator Scope
getScope = gets tstScope

getEntity :: Identifier -> Translator Entity
getEntity name = do
  scope <- getScope
  case Map.lookup name scope of
    Nothing -> throwNameError name
    Just en -> return en

setScope :: Scope -> Translator ()
setScope scope = modify (\tst -> tst { tstScope = scope } )

addToScope :: Identifier -> Entity -> Translator ()
addToScope n en = modify (\tst -> tst { tstScope = Map.insert n en $ tstScope tst })

localScope :: [(Type, Identifier)] -> Translator a -> Translator a
localScope locals x = do
  nonlocals <- getScope
  mapM (\(t, n) -> addToScope n $ Variable t) locals
  x <* setScope nonlocals

getLocation :: Translator Location
getLocation = gets tstLocation

setLocation :: Location -> Translator ()
setLocation loc = modify (\tst -> tst { tstLocation = loc })

withLocation :: Location -> Translator a -> Translator a
withLocation loc x = do
  loc' <- getLocation
  setLocation loc
  x <* setLocation loc'

getFlag :: TranslatorFlag -> Translator Bool
getFlag flag = gets tstFlags >>= (\flags -> return $ flag `elem` flags)

setFlag :: TranslatorFlag -> Bool -> Translator ()
setFlag flag x = modify (\tst -> tst { tstFlags = f $ tstFlags tst }) where
  f flags
    | x == True  = nub $ flag : flags
    | x == False = filter (/= flag) flags

withFlag :: TranslatorFlag -> Translator a -> Translator a
withFlag flag x = setFlag flag True *> x <* setFlag flag False

getEventInstances :: Translator EventInstances
getEventInstances = gets tstEventInstances

getNumOfEventInstances :: Identifier -> Translator Int
getNumOfEventInstances name = do
  insts <- getEventInstances
  case Map.lookup name insts of
    Nothing -> return 0
    Just n  -> return n

addEventInstance :: Identifier -> Translator ()
addEventInstance name = do
  n <- getNumOfEventInstances name
  modify (\tst -> tst { tstEventInstances = Map.insert name (succ n) $ tstEventInstances tst })

getFunctionName :: Translator (Maybe Identifier)
getFunctionName = gets tstFunctionName

setFunctionName :: Maybe Identifier -> Translator ()
setFunctionName name' = modify (\tst -> tst { tstFunctionName = name' })

withFunctionName :: Identifier -> Translator a -> Translator a
withFunctionName name x = do
  name' <- getFunctionName
  setFunctionName $ Just name
  x <* setFunctionName name'

throwErrorGen :: ErrorType -> String -> Translator a
throwErrorGen errt desc = do
  loc <- getLocation
  throwError $ Error errt loc desc

throwSemanticError = throwErrorGen "semantic"
throwTypeError     = throwErrorGen "type"
throwNameError     = throwErrorGen "name"

throwUndefinedNameError :: Identifier -> Translator a
throwUndefinedNameError name = throwNameError desc where
  desc = "name `" ++ name ++ "' is not in scope"

throwInvalidUseError :: Identifier -> String -> String -> Translator a
throwInvalidUseError name et at = throwSemanticError desc where
  desc = "invalid use of " ++ at ++ " `" ++ name ++ "' as " ++ et

throwArgumentsError :: Identifier -> [Type] -> [Type] -> Translator a
throwArgumentsError name eats aats = throwTypeError desc where
  desc = "expected " ++ fse ++ " `" ++ name ++ "', got " ++ fsa
  fse  = if eats == [] then "no arguments for" else se ++ " as arguments for"
  fsa  = if aats == [] then "no arguments"     else sa
  se   = intercalate ", " $ map show eats
  sa   = intercalate ", " $ map show aats

throwOperandsError :: Identifier -> Type -> Type -> Translator a
throwOperandsError op lt rt = throwTypeError desc where
  desc = sl ++ " and " ++ sr ++ " are invalid operands for (" ++ op ++ ")"
  sl   = show lt
  sr   = show rt

throwOperandError :: Identifier -> Type -> Translator a
throwOperandError op xt = throwTypeError desc where
  desc = sx ++ " is invalid operand for (" ++ op ++ ")"
  sx   = show xt

throwUnexpectedTypeError :: String -> Type -> Type -> Translator a
throwUnexpectedTypeError smth at et = throwTypeError $ "expected " ++ se ++ " as " ++ smth ++ ", got " ++ sa where
  se = show et
  sa = show at

throwInitializerTypeError = throwUnexpectedTypeError "initializer"
throwReturnTypeError      = throwUnexpectedTypeError "return value"
throwAssignTypeError      = throwUnexpectedTypeError "assign value"
throwConditionTypeError   = throwUnexpectedTypeError "condition"
throwInvokedTypeError     = throwUnexpectedTypeError "invoke condition"

expectEvent :: Identifier -> ([Type] -> [Identifier] -> [L VariableDefinition] -> [L Statement] -> Translator a) -> Translator a
expectEvent name f = do
  entity <- getEntity name
  case entity of
    Function _ _          -> throwInvalidUseError name "event" "function"
    Variable _            -> throwInvalidUseError name "event" "variable"
    Event    ats ans fs b -> f ats ans fs b

expectFunction :: Identifier -> (Type -> [Type] -> Translator a) -> Translator a
expectFunction name f = do
  entity <- getEntity name
  case entity of
    Event    _ _ _ _ -> throwInvalidUseError name "function" "event"
    Variable _       -> throwInvalidUseError name "function" "variable"
    Function rt ats  -> f rt ats

expectVariable :: Identifier -> (Type -> Translator a) -> Translator a
expectVariable name f = do
  entity <- getEntity name
  case entity of
    Event    _ _ _ _ -> throwInvalidUseError name "variable" "event"
    Function _ _     -> throwInvalidUseError name "variable" "function"
    Variable t       -> f t
