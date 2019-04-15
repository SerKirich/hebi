{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Translator (translate) where

import Debug.Trace

import Control.Monad.Except
import Control.Monad.State
import Control.Monad

import Location

import Translator.Core
import Translator.Types
import Translator.TypeSystem
import Translator.TypeChecker

import qualified Native.AST  as Native
import qualified Foreign.AST as Foreign

class Translatable a b | a -> b where
  translate :: a -> Either Error b
  translate x = runExcept $ evalStateT (runTranslator $ transM x) initTranslatorState

  transML :: Located a -> Translator b
  transML (Located loc x) = withLocation loc $ transM x

  transM :: a -> Translator b

instance Translatable Native.Program Foreign.Program where
  transM (Native.Program xtrns units regs) = do
    mapM transML xtrns
    globals                   <- mapM transML units
    (extraGlobals, loopParts) <- (\(x, y) -> (concat x, concat y)) <$> (liftM unzip $ mapM transML regs)
    let loop  = Foreign.UFunction $ Foreign.DFunction Foreign.TVoid "loop" [] loopParts
    return $ Foreign.Program $ globals ++ extraGlobals ++ [loop]

instance Translatable Native.TranslationUnit Foreign.TranslationUnit where
  transM (Native.UFunction f) = Foreign.UFunction <$> transM f
  transM (Native.UVariable v) = Foreign.UVariable <$> transM v
  transM (Native.UEvent    e) = Foreign.UEmpty    <$  transM e

instance Translatable Native.ExternDeclaration () where
  transM (Native.DExternFunction t name ats) = addToScope name $ Function (toType t) (map toType ats)
  transM (Native.DExternVariable t name    ) = addToScope name $ Variable (toType t)

instance Translatable Native.EventRegistration ([Foreign.TranslationUnit], [Foreign.Statement]) where
  transM (Native.REvent name as) = expectEvent name $ \eats ans fs b -> do
    addEventInstance name
    aats <- mapM (getType . fromLocated) as
    if aats /= eats
    then throwArgumentsError name eats aats
    else localScope (zip aats ans ++ map (\(Located _ (Native.DValue t n _)) -> (toType t, n)) fs) $ do
      as' <- mapM transML as
      b'  <- mapM transML b
      fs' <- mapM transML fs
      (fns, fns') <- liftM unzip $ mapM (getNames name) fs'
      let extraGlobals = map (renameVDef (zip fns fns') $ zip ans as') fs'
      let loopParts    = map (inlineStmt $ zip ans as' ++ zip fns (map Foreign.EName fns')) b'
      return $ (map Foreign.UVariable extraGlobals, loopParts) where
        getNames eventName (Foreign.DValue _ name _) = do
          n <- getNumOfEventInstances eventName
          return (name, eventName ++ show n ++ "_" ++ name)

instance Translatable Native.EventDefinition () where
  transM (Native.DEvent name as fs i h) = do
    addToScope name $ Event (map toType ats) ans fs b' where
      ats = fst . unzip $ as
      ans = snd . unzip $ as

      b' = map (whenToIf h) i

instance Translatable Native.FunctionDefinition Foreign.FunctionDefinition where
  transM (Native.DFunction t name as b) = do
    addToScope name (Function (toType t) $ map toType $ fst . unzip $ as)
    withFunctionName name $ localScope (map (\(at, an) -> (toType at, an)) as) $ do
      Foreign.DFunction <$> transM t <*> pure name <*> mapM transMP as <*> checkReturn b where
        checkReturn b = do
          if toType t == Void
          then mapM transML b
          else do
            if returnsAreEverywhereIn b
            then mapM transML b
            else throwReturnTypeError Void (toType t)

        returnsAreEverywhereIn = (foldl foldf False) . (map fromLocated)

        foldf False (Native.SIfElse _ p n) = (returnsAreEverywhereIn p) && (returnsAreEverywhereIn n)
        foldf False (Native.SWhile  _   b) = (returnsAreEverywhereIn b)

        foldf False (Native.SReturn _) = True

        foldf False _ = False
        foldf True  _ = True

        transMP (at, an) = (,) <$> transM at <*> pure an

instance Translatable Native.VariableDefinition Foreign.VariableDefinition where
  transM (Native.DValue t name x) = do
      addToScope name (Variable $ toType t)
      requireType (toType t) (fromLocated x) throwInitializerTypeError
      Foreign.DValue <$> transM t <*> pure name <*> transML x

  transM (Native.DArray t name xs) = do
    addToScope name (Variable $ toType t)
    case t of
      Native.TPointer t' -> do
        mapM (\x -> requireType (toType t') (fromLocated x) throwInitializerTypeError) xs
        Foreign.DArray <$> transM t <*> pure name <*> mapM transML xs

instance Translatable Native.Statement Foreign.Statement where
  transM (Native.SDefine      x) = (Foreign.SEvaluate . Foreign.EDefine) <$> transML x
  transM (Native.SAssign name x) = expectVariable name $ \t -> do
    requireType t (fromLocated x) $ throwAssignTypeError
    (Foreign.SEvaluate . (Foreign.EBinOp "=" $ Foreign.EName name)) <$> transML x

  transM (Native.SIfElse c p n) = do
    requireType Bool (fromLocated c) throwConditionTypeError
    Foreign.SIfElse <$> transML c <*> mapM transML p <*> mapM transML n

  transM (Native.SWhile c b) = withFlag FlagLoop $ do
    requireType Bool (fromLocated c) throwConditionTypeError
    Foreign.SWhile <$> transML c <*> mapM transML b

  transM (Native.SEvaluate x) = Foreign.SEvaluate <$> transML x
  transM (Native.SReturn   x) = do
    name' <- getFunctionName
    case name' of
      Nothing -> throwSemanticError "`return' statement outside function definition"
      Just name -> expectFunction name $ \rt _ -> do
        requireType rt (fromLocated x) throwReturnTypeError
        Foreign.SReturn <$> transML x

  transM (Native.SWhen x) = do
    inside <- getFlag FlagIvkd
    when (not inside) $ throwSemanticError "`when' statement outside `invoked' section"
    requireType Bool (fromLocated x) throwInvokedTypeError
    pure $ Foreign.SEvaluate Foreign.ENull

  transM Native.SContinue = do
    inside <- getFlag FlagLoop
    when (not inside) $ throwSemanticError "`continue' statement outside loop"
    pure Foreign.SContinue

  transM Native.SBreak = do
    inside <- getFlag FlagLoop
    when (not inside) $ throwSemanticError "`break' statement outside loop"
    pure Foreign.SBreak

instance Translatable Native.Expression Foreign.Expression where
  transM (Native.ECast t x) = Foreign.ECast  <$> transM t <*> transML x

  transM e@(Native.ECond   c p n) = checkType e >> Foreign.ECond  <$> transML c       <*> transML p <*> transML n
  transM e@(Native.EBinOp op l r) = checkType e >> Foreign.EBinOp <$> pure (binOp op) <*> transML l <*> transML r
  transM e@(Native.EUnOp  op   x) = checkType e >> Foreign.EUnOp  <$> pure op         <*> transML x
  transM e@(Native.ECall name as) = checkType e >> Foreign.ECall  <$> pure name       <*> mapM transML as
  transM e@(Native.EName name   ) = checkType e >> Foreign.EName  <$> pure name

  transM (Native.EInt   x) = pure (Foreign.EInt   x)
  transM (Native.EFloat x) = pure (Foreign.EFloat x)
  transM (Native.EBool  x) = pure (Foreign.EBool  x)
  transM (Native.EChar  x) = pure (Foreign.EChar  x)
  transM (Native.EStr   x) = pure (Foreign.EStr   x)

  transM (Native.EPin False x) = pure (Foreign.EInt                x)
  transM (Native.EPin True  x) = pure (Foreign.EName $ "A" ++ show x)

instance Translatable Native.TypeIdentifier Foreign.TypeIdentifier where
  transM (Native.TPointer t) = Foreign.TPointer <$> transM t

  transM Native.TPin    = pure Foreign.TUInt8
  transM Native.TInt8   = pure Foreign.TInt8
  transM Native.TUInt8  = pure Foreign.TUInt8
  transM Native.TInt16  = pure Foreign.TInt16
  transM Native.TUInt16 = pure Foreign.TUInt16
  transM Native.TInt32  = pure Foreign.TInt32
  transM Native.TUInt32 = pure Foreign.TUInt32
  transM Native.TFloat  = pure Foreign.TFloat
  transM Native.TBool   = pure Foreign.TBool
  transM Native.TChar   = pure Foreign.TChar
  transM Native.TVoid   = pure Foreign.TVoid

whenToIf :: [Located Native.Statement] -> Located Native.Statement -> Located Native.Statement
whenToIf h = Native.mapStmtL onWhen where
  onWhen (Native.SWhen c) = Native.SIfElse c h []
  onWhen x = x

renameVDef :: [(Foreign.Identifier, Foreign.Identifier)] -> [(Foreign.Identifier, Foreign.Expression)] -> Foreign.VariableDefinition -> Foreign.VariableDefinition
renameVDef lns ctx (Foreign.DValue t name x) = case lookup name lns of
  Nothing    -> inlineVDef ctx $ Foreign.DValue t name  x
  Just name' -> inlineVDef ctx $ Foreign.DValue t name' x

inlineStmt :: [(Foreign.Identifier, Foreign.Expression)] -> Foreign.Statement -> Foreign.Statement
inlineStmt ctx = Foreign.mapStmt onExpr where
  onExpr (Foreign.SIfElse c p n) = Foreign.SIfElse   (inlineExpr ctx c) p n
  onExpr (Foreign.SWhile  c   b) = Foreign.SWhile    (inlineExpr ctx c) b
  onExpr (Foreign.SEvaluate   x) = Foreign.SEvaluate (inlineExpr ctx x)
  onExpr x = x

inlineExpr :: [(Foreign.Identifier, Foreign.Expression)] -> Foreign.Expression -> Foreign.Expression
inlineExpr ctx = Foreign.mapExpr onName where
  onName (Foreign.EDefine vd) = Foreign.EDefine $ inlineVDef ctx vd
  onName (Foreign.EName name) = case lookup name ctx of
    Nothing -> Foreign.EName name
    Just x  -> x
  onName x = x

inlineVDef :: [(Foreign.Identifier, Foreign.Expression)] -> Foreign.VariableDefinition -> Foreign.VariableDefinition
inlineVDef ctx (Foreign.DValue t name x ) = Foreign.DValue t name      (inlineExpr ctx  x )
inlineVDef ctx (Foreign.DArray t name xs) = Foreign.DArray t name (map (inlineExpr ctx) xs)

binOp :: String -> String
binOp op = let table = []
           in case lookup op table of
                Just op' -> op'
                Nothing  -> op
