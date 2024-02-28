{-# LANGUAGE LambdaCase #-}

module Treeanize (toAst, FruExpr (..), FruStmt (..)) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Scientific (Scientific)
import Data.Set (Set, singleton)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, token, try)
  , Parsec
  , between
  , choice
  , many
  , sepBy
  , single
  , (<|>)
  )
import Text.Megaparsec.Error (ErrorItem (Label))
import Tokenize (FruToken (..))


data FruExpr
  = ExLiteralNumber Scientific
  | ExLiteralBool Bool
  | ExLiteralString String
  | ExVariable String
  | ExCall FruExpr [FruExpr]
  | ExBinary String FruExpr FruExpr
  | ExFnDef [String] FruStmt
  deriving (Show, Eq)


data FruStmt
  = StComposite [FruStmt]
  | StExpr FruExpr
  | StLet String FruExpr
  | StSet String FruExpr
  | StIf FruExpr FruStmt FruStmt
  | StWhile FruExpr FruStmt
  | StReturn FruExpr
  | StBreak
  | StContinue
  | StOpDef String String String String String FruStmt
  -- operator ident, left arg ident, left arg type ident, right arg ident, right arg type ident, body
  deriving (Show, Eq)


-- helpers

makeErrSet :: String -> Set (ErrorItem FruToken)
makeErrSet = singleton . Label . NonEmpty.fromList


-- parser

type ParserStmt = Parsec Void [FruToken] FruStmt


type ParserExpr = Parsec Void [FruToken] FruExpr


toAst :: ParserStmt
toAst = program
  where
    program :: ParserStmt
    program = do
      stmts <- many stmt <* eof
      return $ StComposite stmts

    stmt :: ParserStmt
    stmt =
      choice
        [ try blockStmt
        , try letStmt
        , try setStmt
        , try ifElseStmt
        , try ifStmt
        , try whileStmt
        , try returnStmt
        , try breakStmt
        , try continueStmt
        , try operatorDefStmt
        , try exprStmt
        ]

    blockStmt :: ParserStmt
    blockStmt = do
      stmts <-
        between
          (single TkBraceOpen)
          (single TkBraceClose)
          (many stmt)
      return $
        if length stmts == 1
          then head stmts
          else StComposite stmts

    semicolon = single TkSemiColon

    exprStmt :: ParserStmt
    exprStmt = do
      ex <- expr
      _ <- semicolon
      return $ StExpr ex

    letStmt :: ParserStmt
    letStmt = do
      _ <- single TkLet
      name <- token (\case TkIdent x -> Just x; _ -> Nothing) (makeErrSet "identifier")
      _ <- single (TkOp "=")
      value <- expr
      _ <- semicolon
      return $ StLet name value

    setStmt :: ParserStmt
    setStmt = do
      name <- token (\case TkIdent x -> Just x; _ -> Nothing) (makeErrSet "identifier")
      _ <- single (TkOp "=")
      value <- expr
      _ <- semicolon
      return $ StSet name value

    ifStmt :: ParserStmt
    ifStmt = do
      _ <- single TkIf
      cond <- expr
      thenBody <- blockStmt
      return $ StIf cond thenBody (StComposite [])

    ifElseStmt :: ParserStmt
    ifElseStmt = do
      _ <- single TkIf
      cond <- expr
      thenBody <- blockStmt
      _ <- single TkElse
      StIf cond thenBody <$> (blockStmt <|> ifElseStmt <|> ifStmt)

    whileStmt :: ParserStmt
    whileStmt = do
      _ <- single TkWhile
      cond <- expr
      StWhile cond <$> blockStmt

    returnStmt :: ParserStmt
    returnStmt = do
      _ <- single TkReturn
      value <- expr
      _ <- semicolon
      return $ StReturn value

    breakStmt :: ParserStmt
    breakStmt = StBreak <$ single TkBreak <* semicolon

    continueStmt :: ParserStmt
    continueStmt = StContinue <$ single TkContinue <* semicolon

    operatorDefStmt :: ParserStmt
    operatorDefStmt = do
      _ <- single TkOpDef
      ident <- token (\case TkOp x -> Just x; _ -> Nothing) (makeErrSet "identifier")
      _ <- single TkParenOpen

      leftIdent <- token (\case TkIdent x -> Just x; _ -> Nothing) (makeErrSet "identifier")
      _ <- single TkColon
      leftType <- token (\case TkIdent x -> Just x; _ -> Nothing) (makeErrSet "type-identifier")

      _ <- single TkComma

      rightIdent <- token (\case TkIdent x -> Just x; _ -> Nothing) (makeErrSet "identifier")
      _ <- single TkColon
      rightType <- token (\case TkIdent x -> Just x; _ -> Nothing) (makeErrSet "type-identifier")

      _ <- single TkParenClose

      StOpDef ident leftIdent leftType rightIdent rightType <$> blockStmt

    expr :: ParserExpr
    expr =
      choice
        [ literalNumber
        , literalBool
        , literalString
        , variable
        , try binary
        , try call
        , try paren
        , try fnDef
        ]
      where
        literalNumber :: ParserExpr
        literalNumber = do
          value <- token (\case TkNumber x -> Just x; _ -> Nothing) (makeErrSet "number")
          return $ ExLiteralNumber value

        literalBool :: ParserExpr
        literalBool = do
          value <- token (\case TkBool x -> Just x; _ -> Nothing) (makeErrSet "bool")
          return $ ExLiteralBool value

        literalString :: ParserExpr
        literalString = do
          value <- token (\case TkString x -> Just x; _ -> Nothing) (makeErrSet "string")
          return $ ExLiteralString value

        variable :: ParserExpr
        variable = do
          ident <- token (\case TkIdent x -> Just x; _ -> Nothing) (makeErrSet "identifier")
          return $ ExVariable ident

        paren :: ParserExpr
        paren =
          between
            (single TkParenOpen)
            (single TkParenClose)
            expr

        call :: ParserExpr
        call = do
          what <- paren
          args <-
            between
              (single TkParenOpen)
              (single TkParenClose)
              (sepBy expr (single TkComma))
          return $ ExCall what args

        binary :: ParserExpr
        binary = do
          left <- paren
          op <- token (\case TkOp x -> Just x; _ -> Nothing) (makeErrSet "operator")
          ExBinary op left <$> paren

        fnDef :: ParserExpr
        fnDef = do
          _ <- single TkFn
          args <-
            between
              (single TkParenOpen)
              (single TkParenClose)
              ( sepBy
                  (token (\case TkIdent x -> Just x; _ -> Nothing) (makeErrSet "identifier"))
                  (single TkComma)
              )

          ExFnDef args <$> blockStmt
