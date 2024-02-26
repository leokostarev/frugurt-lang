{-# LANGUAGE LambdaCase #-}

module Treeanize (toAst, FruExpr (..), FruStmt (..)) where

import qualified Data.Set as Set
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
import Tokenize (FruToken (..))


data FruExpr
  = ExLiteralInt Int
  | ExLiteralBool Bool
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
  deriving (Show, Eq)


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
        , try exprStmt
        ]

    blockStmt :: ParserStmt
    blockStmt = do
      stmts <- between (single TkBraceOpen) (single TkBraceClose) (many stmt)
      return $
        if length stmts == 1
          then head stmts
          else StComposite stmts

    semicolon = single TkSemiColon

    exprStmt = do
      ex <- expr
      _ <- semicolon
      return $ StExpr ex

    letStmt = do
      _ <- single TkLet
      name <- token (\case TkIdent x -> Just x; _ -> Nothing) Set.empty
      _ <- single (TkOp "=")
      value <- expr
      _ <- semicolon
      return $ StLet name value

    setStmt = do
      name <- token (\case TkIdent x -> Just x; _ -> Nothing) Set.empty
      _ <- single (TkOp "=")
      value <- expr
      _ <- semicolon
      return $ StSet name value

    ifStmt = do
      _ <- single TkIf
      cond <- expr
      thenBody <- blockStmt
      return $ StIf cond thenBody (StComposite [])

    ifElseStmt = do
      _ <- single TkIf
      cond <- expr
      thenBody <- blockStmt
      _ <- single TkElse
      StIf cond thenBody <$> (blockStmt <|> ifElseStmt <|> ifStmt)

    whileStmt = do
      _ <- single TkWhile
      cond <- expr
      StWhile cond <$> blockStmt

    returnStmt = do
      _ <- single TkReturn
      value <- expr
      _ <- semicolon
      return $ StReturn value

    breakStmt = StBreak <$ single TkBreak <* semicolon

    continueStmt = StContinue <$ single TkContinue <* semicolon

    expr :: ParserExpr
    expr =
      choice
        [ literalInt
        , literalBool
        , variable
        , try binary
        , try call
        , try paren
        , try fnDef
        ]
      where
        literalInt :: ParserExpr
        literalInt = do
          value <- token (\case TkInt x -> Just x; _ -> Nothing) Set.empty
          return $ ExLiteralInt value

        literalBool :: ParserExpr
        literalBool = do
          value <- token (\case TkBool x -> Just x; _ -> Nothing) Set.empty
          return $ ExLiteralBool value

        variable :: ParserExpr
        variable = do
          ident <- token (\case TkIdent x -> Just x; _ -> Nothing) Set.empty
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
          op <- token (\case TkOp x -> Just x; _ -> Nothing) Set.empty
          ExBinary op left <$> paren

        fnDef :: ParserExpr
        fnDef = do
          _ <- single TkFn
          args <-
            between
              (single TkParenOpen)
              (single TkParenClose)
              ( sepBy
                  (token (\case TkIdent x -> Just x; _ -> Nothing) Set.empty)
                  (single TkComma)
              )

          ExFnDef args <$> blockStmt
