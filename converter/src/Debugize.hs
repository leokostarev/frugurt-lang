{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Debugize (toDbgStrStmt) where

import Data.List (intercalate)
import Treeanize (FruExpr (..), FruField (..), FruStmt (..), FruWatch (..))


getSpace :: Int -> String
getSpace indent = concat $ replicate indent "|    "


toDbgWatch :: Int -> FruWatch -> String
toDbgWatch indent (FruWatch fields body) =
  getSpace indent
    ++ "Watch:\n"
    ++ getSpace (indent + 1)
    ++ "fields: "
    ++ intercalate ", " fields
    ++ "\n"
    ++ getSpace (indent + 1)
    ++ "body:\n"
    ++ toDbgStrStmt (indent + 2) body


toDbgStrField :: Int -> FruField -> String
toDbgStrField indent (FruField isPub name typeIdent) =
  getSpace indent
    ++ (if isPub then "pub " else "")
    ++ name
    ++ ( case typeIdent of
          Nothing -> ""
          Just ident -> " : " ++ ident
       )
    ++ "\n"


toDbgStrProperty :: Int -> String -> a -> (Int -> a -> String) -> String
toDbgStrProperty indent name value toString =
  getSpace (indent + 1)
    ++ name
    ++ ":\n"
    ++ toString (indent + 2) value


toDbgStrStr :: Int -> String -> String
toDbgStrStr indent str = getSpace indent ++ str ++ "\n"


toDbgStrStmt :: Int -> FruStmt -> String
toDbgStrStmt indent = \case
  StComposite stmts ->
    getSpace indent
      ++ "Composite:\n"
      ++ concatMap (toDbgStrStmt (indent + 1)) stmts
  StExpr e ->
    getSpace indent
      ++ "Expression:\n"
      ++ toDbgStrExpr (indent + 1) e
  StLet ident e ->
    getSpace indent
      ++ "Let:\n"
      ++ toDbgStrProperty indent "ident" ident toDbgStrStr
      ++ toDbgStrProperty indent "what" e toDbgStrExpr
  StSet path e ->
    getSpace indent
      ++ "Set:\n"
      ++ toDbgStrProperty indent "path" (intercalate "." path) toDbgStrStr
      ++ toDbgStrProperty indent "what" e toDbgStrExpr
  StIf cond thenBody elseBody ->
    getSpace indent
      ++ "If:\n"
      ++ toDbgStrProperty indent "cond" cond toDbgStrExpr
      ++ toDbgStrProperty indent "then" thenBody toDbgStrStmt
      ++ toDbgStrProperty indent "else" elseBody toDbgStrStmt
  StWhile cond body ->
    getSpace indent
      ++ "While:\n"
      ++ toDbgStrProperty indent "cond" cond toDbgStrExpr
      ++ toDbgStrProperty indent "body" body toDbgStrStmt
  StReturn e ->
    getSpace indent
      ++ "Return:\n"
      ++ toDbgStrExpr (indent + 1) e
  StBlockReturn e ->
    getSpace indent
      ++ "BlockReturn:\n"
      ++ toDbgStrExpr (indent + 1) e
  StBreak ->
    getSpace indent
      ++ "Break\n"
  StContinue ->
    getSpace indent
      ++ "Continue\n"
  StOperator op leftIdent leftType rightIdent rightType body ->
    getSpace indent
      ++ "Operator:\n"
      ++ toDbgStrProperty indent "op" op toDbgStrStr
      ++ toDbgStrProperty indent "left" (leftIdent ++ " : " ++ leftType) toDbgStrStr
      ++ toDbgStrProperty indent "right" (rightIdent ++ " : " ++ rightType) toDbgStrStr
      ++ toDbgStrProperty indent "body" body toDbgStrStmt
  StType typeType ident fields watches ->
    getSpace indent
      ++ "Type:\n"
      ++ toDbgStrProperty indent "type" typeType toDbgStrStr
      ++ toDbgStrProperty indent "ident" ident toDbgStrStr
      ++ getSpace (indent + 1)
      ++ "fields:\n"
      ++ concatMap (toDbgStrField (indent + 2)) fields
      ++ getSpace (indent + 1)
      ++ "watches:\n"
      ++ concatMap (toDbgWatch (indent + 2)) watches


toDbgStrExpr :: Int -> FruExpr -> String
toDbgStrExpr indent = \case
  ExLiteralNumber n -> getSpace indent ++ show n ++ "\n"
  ExLiteralBool b -> getSpace indent ++ show b ++ "\n"
  ExLiteralString s -> getSpace indent ++ show s ++ "\n"
  ExVariable v -> getSpace indent ++ v ++ "\n"
  ExCall e es ->
    getSpace indent
      ++ "Call:\n"
      ++ toDbgStrProperty indent "what" e toDbgStrExpr
      ++ getSpace (indent + 1)
      ++ "args:\n"
      ++ concatMap (toDbgStrExpr (indent + 2)) es
  ExCurryCall e es ->
    getSpace indent
      ++ "CurryCall:\n"
      ++ toDbgStrProperty indent "what" e toDbgStrExpr
      ++ getSpace (indent + 1)
      ++ "args:\n"
      ++ concatMap (toDbgStrExpr (indent + 2)) es
  ExBinary op e1 e2 ->
    getSpace indent
      ++ "Binary:\n"
      ++ toDbgStrProperty indent "op" op toDbgStrStr
      ++ toDbgStrProperty indent "left" e1 toDbgStrExpr
      ++ toDbgStrProperty indent "right" e2 toDbgStrExpr
  ExFunction args body ->
    getSpace indent
      ++ "Function:\n"
      ++ toDbgStrProperty indent "args" (intercalate ", " args) toDbgStrStr
      ++ toDbgStrProperty indent "body" body toDbgStrStmt
  ExInstantiation e es ->
    getSpace indent
      ++ "Instantiation:\n"
      ++ toDbgStrProperty indent "what" e toDbgStrExpr
      ++ getSpace (indent + 1)
      ++ "args:\n"
      ++ concatMap (toDbgStrExpr (indent + 2)) es
  ExFieldAccess e f ->
    getSpace indent
      ++ "FieldAccess:\n"
      ++ toDbgStrProperty indent "what" e toDbgStrExpr
      ++ toDbgStrProperty indent "field" f toDbgStrStr