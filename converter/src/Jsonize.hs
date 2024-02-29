module Jsonize (toJsonExpr, toJsonStmt, toString) where

import Data.List (intercalate)
import Data.Scientific (toRealFloat)
import Treeanize (FruExpr (..), FruStmt (..))


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
  ExLiteralString s ->
    Object
      [ ("node", Str "literal")
      , ("value", Str s)
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
      [ ("node", Str "function")
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
  StLet ident expression ->
    Object
      [ ("node", Str "let")
      , ("ident", Str ident)
      , ("value", toJsonExpr expression)
      ]
  StSet ident expression ->
    Object
      [ ("node", Str "set")
      , ("ident", Str ident)
      , ("value", toJsonExpr expression)
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
  StReturn expression ->
    Object
      [ ("node", Str "return")
      , ("value", toJsonExpr expression)
      ]
  StBlockReturn expression ->
    Object
      [ ("node", Str "block_return")
      , ("value", toJsonExpr expression)
      ]
  StBreak -> Object [("node", Str "break")]
  StContinue -> Object [("node", Str "continue")]
  StOpDef op left_arg left_type right_arg right_type body ->
    Object
      [ ("node", Str "operator")
      , ("ident", Str op)
      , ("left_arg", Str left_arg)
      , ("left_type", Str left_type)
      , ("right_arg", Str right_arg)
      , ("right_type", Str right_type)
      , ("body", toJsonStmt body)
      ]


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