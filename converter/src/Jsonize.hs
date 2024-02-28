module Jsonize (toJsonExpr, toJsonStmt, toString) where

import Data.List (intercalate)
import Treeanize (FruExpr (..), FruStmt (..))
import Data.Scientific (toRealFloat)


data JSON
  = Number Double
  | Bool Bool
  | Str String
  | Array [JSON]
  | Object [(String, JSON)]
  deriving (Show, Eq)


toJsonExpr :: FruExpr -> JSON
toJsonExpr expr = case expr of
  ExLiteralNumber i ->
    Object
      [ ("node", Str "literal")
      , ("value", Number $ toRealFloat i)
      ]
  ExLiteralBool b ->
    Object
      [ ("node", Str "literal")
      , ("value", Bool b)
      ]
  ExVariable s ->
    Object
      [ ("node", Str "variable")
      , ("ident", Str s)
      ]
  ExCall what args ->
    Object
      [ ("node", Str "call")
      , ("what", toJsonExpr what)
      , ("args", Array $ map toJsonExpr args)
      ]
  ExBinary op left right ->
    Object
      [ ("node", Str "binary")
      , ("operator", Str op)
      , ("left", toJsonExpr left)
      , ("right", toJsonExpr right)
      ]
  ExFnDef args body ->
    Object
      [ ("node", Str "fn_def")
      , ("args", Array $ map Str args)
      , ("body", toJsonStmt body)
      ]


toJsonStmt :: FruStmt -> JSON
toJsonStmt stmt = case stmt of
  StComposite body ->
    Object
      [ ("node", Str "composite")
      , ("body", Array $ map toJsonStmt body)
      ]
  StExpr expression ->
    Object
      [ ("node", Str "expression")
      , ("value", toJsonExpr expression)
      ]
  StLet ident value ->
    Object
      [ ("node", Str "let")
      , ("ident", Str ident)
      , ("value", toJsonExpr value)
      ]
  StSet ident value ->
    Object
      [ ("node", Str "set")
      , ("ident", Str ident)
      , ("value", toJsonExpr value)
      ]
  StIf cond thenBody elseBody ->
    Object
      [ ("node", Str "if")
      , ("cond", toJsonExpr cond)
      , ("then", toJsonStmt thenBody)
      , ("else", toJsonStmt elseBody)
      ]
  StWhile cond body ->
    Object
      [ ("node", Str "while")
      , ("cond", toJsonExpr cond)
      , ("body", toJsonStmt body)
      ]
  StReturn value ->
    Object
      [ ("node", Str "return")
      , ("value", toJsonExpr value)
      ]
  StBreak -> Object [("node", Str "break")]
  StContinue -> Object [("node", Str "continue")]


toString :: JSON -> String
toString (Number i) = show i
toString (Bool b)
  | b = "true"
  | otherwise = "false"
toString (Str s) = show s
toString (Array xs) =
  "["
    ++ intercalate ", " (map toString xs)
    ++ "]"
toString (Object xs) =
  "{"
    ++ intercalate
      ", "
      (map (\(k, v) -> "\"" ++ k ++ "\"" ++ ": " ++ toString v) xs)
    ++ "}"