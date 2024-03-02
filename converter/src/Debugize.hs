{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Debugize (toDbgStrStmt) where

import Data.List (intercalate)
import Treeanize (FruExpr (..), FruStmt (..))


getSpace :: Int -> String
getSpace indent = concat $ replicate indent "|    "


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
      ++ getSpace (indent + 1)
      ++ "name: "
      ++ ident
      ++ "\n"
      ++ getSpace (indent + 1)
      ++ "what:\n"
      ++ toDbgStrExpr (indent + 2) e
  StSet ident e ->
    getSpace indent
      ++ "Set:\n"
      ++ getSpace (indent + 1)
      ++ "name: "
      ++ ident
      ++ "\n"
      ++ getSpace (indent + 1)
      ++ "what:\n"
      ++ toDbgStrExpr (indent + 2) e
  StIf cond thenBody elseBody ->
    getSpace indent
      ++ "If:\n"
      ++ getSpace (indent + 1)
      ++ "cond:\n"
      ++ toDbgStrExpr (indent + 2) cond
      ++ getSpace (indent + 1)
      ++ "then:\n"
      ++ toDbgStrStmt (indent + 2) thenBody
      ++ getSpace (indent + 1)
      ++ "else:\n"
      ++ toDbgStrStmt (indent + 2) elseBody
  StWhile cond body ->
    getSpace indent
      ++ "While:\n"
      ++ getSpace (indent + 1)
      ++ "cond:\n"
      ++ toDbgStrExpr (indent + 2) cond
      ++ getSpace (indent + 1)
      ++ "body:\n"
      ++ toDbgStrStmt (indent + 2) body
  StReturn e ->
    getSpace indent
      ++ "Return:\n"
      ++ getSpace indent
      ++ "what: "
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
      ++ getSpace (indent + 1)
      ++ "op:\n"
      ++ getSpace (indent + 2)
      ++ op
      ++ "\n"
      ++ getSpace (indent + 1)
      ++ "left:\n"
      ++ getSpace (indent + 2)
      ++ leftIdent
      ++ " : "
      ++ leftType
      ++ "\n"
      ++ getSpace (indent + 1)
      ++ "right:\n"
      ++ getSpace (indent + 2)
      ++ rightIdent
      ++ " : "
      ++ rightType
      ++ "\n"
      ++ getSpace (indent + 1)
      ++ "body:\n"
      ++ toDbgStrStmt (indent + 2) body
  StType t ident fields ->
    getSpace indent
      ++ "Type:\n"
      ++ getSpace (indent + 1)
      ++ "type:\n"
      ++ getSpace (indent + 2)
      ++ t
      ++ "\n"
      ++ getSpace (indent + 1)
      ++ "ident:\n"
      ++ getSpace (indent + 2)
      ++ ident
      ++ "\n"
      ++ getSpace (indent + 1)
      ++ "fields:\n"
      ++ getSpace (indent + 2)
      ++ intercalate ", " fields
      ++ "\n"


toDbgStrExpr :: Int -> FruExpr -> String
toDbgStrExpr indent = \case
  ExLiteralNumber n -> getSpace indent ++ show n ++ "\n"
  ExLiteralBool b -> getSpace indent ++ show b ++ "\n"
  ExLiteralString s -> getSpace indent ++ show s ++ "\n"
  ExVariable v -> getSpace indent ++ v ++ "\n"
  ExCall e es ->
    getSpace indent
      ++ "Call:\n"
      ++ getSpace (indent + 1)
      ++ "what:\n"
      ++ toDbgStrExpr (indent + 2) e
      ++ getSpace (indent + 1)
      ++ "args:\n"
      ++ concatMap (toDbgStrExpr (indent + 2)) es
  ExCurryCall e es ->
    getSpace indent
      ++ "CurryCall:\n"
      ++ getSpace (indent + 1)
      ++ "what:\n"
      ++ toDbgStrExpr (indent + 2) e
      ++ getSpace (indent + 1)
      ++ "args:\n"
      ++ concatMap (toDbgStrExpr (indent + 2)) es
  ExBinary op e1 e2 ->
    getSpace indent
      ++ "Binary:\n"
      ++ getSpace (indent + 1)
      ++ "op: "
      ++ op
      ++ "\n"
      ++ getSpace (indent + 1)
      ++ "left:\n"
      ++ toDbgStrExpr (indent + 2) e1
      ++ getSpace (indent + 1)
      ++ "right:\n"
      ++ toDbgStrExpr (indent + 2) e2
  ExFunction args body ->
    getSpace indent
      ++ "Function:\n"
      ++ getSpace (indent + 1)
      ++ "args: "
      ++ intercalate ", " args
      ++ "\n"
      ++ getSpace (indent + 1)
      ++ "body:\n"
      ++ toDbgStrStmt (indent + 2) body
  ExInstantiation e es ->
    getSpace indent
      ++ "Instantiation:\n"
      ++ getSpace (indent + 1)
      ++ "what:\n"
      ++ toDbgStrExpr (indent + 2) e
      ++ getSpace (indent + 1)
      ++ "args:\n"
      ++ concatMap (toDbgStrExpr (indent + 2)) es
  ExFieldAccess e f ->
    getSpace indent
      ++ "FieldAccess:\n"
      ++ getSpace (indent + 1)
      ++ "what:\n"
      ++ toDbgStrExpr (indent + 2) e
      ++ getSpace (indent + 1)
      ++ "field: "
      ++ f
      ++ "\n"
