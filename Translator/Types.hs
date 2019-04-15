module Translator.Types where

import Data.Map (Map)

import Location

import Native.AST

import Translator.TypeSystem

data Error = Error {
    errorType        :: ErrorType,
    errorLocation    :: Location,
    errorDescription :: String
  }

instance Show Error where
  show err = show l ++ ": " ++ t ++ " error:\n\t" ++ d where
    t = errorType        err
    l = errorLocation    err
    d = errorDescription err

data Entity
  = Event         [Type] [Identifier] [Located VariableDefinition] [Located Statement]
  | Function Type [Type]
  | Variable Type

type Scope          = Map Identifier Entity
type EventInstances = Map Identifier Int
type ErrorType      = String
