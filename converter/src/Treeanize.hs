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
  , optional
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
  | ExCurryCall FruExpr [FruExpr]
  | ExBinary String FruExpr FruExpr
  | ExFnDef [String] FruStmt
  | ExInstantiation FruExpr [FruExpr] -- type object * field values
  | ExFieldAccess FruExpr String
  deriving (Show, Eq)


data FruStmt
  = StComposite [FruStmt]
  | StExpr FruExpr
  | StLet String FruExpr
  | StSet String FruExpr
  | StIf FruExpr FruStmt FruStmt
  | StWhile FruExpr FruStmt
  | StReturn FruExpr
  | StBlockReturn FruExpr
  | StBreak
  | StContinue
  | StOperator String String String String String FruStmt -- operator ident * left arg ident * left arg type ident * right arg ident * right arg type ident * body
  | StType String String [String] -- ("struct") * ident * field idents
  deriving (Show, Eq)


-- helpers

makeErrSet :: String -> Set (ErrorItem FruToken)
makeErrSet = singleton . Label . NonEmpty.fromList


makeComposite :: [FruStmt] -> FruStmt
makeComposite stmts
  | length stmts == 1 = head stmts
  | otherwise = StComposite stmts


-- parser

type ParserStmt = Parsec Void [FruToken] FruStmt


type ParserExpr = Parsec Void [FruToken] FruExpr


identifier :: Parsec Void [FruToken] String
identifier = token (\case TkIdent x -> Just x; _ -> Nothing) (makeErrSet "identifier")


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
        [ blockStmt
        , try letStmt
        , try setStmt
        , try ifElseStmt
        , try ifStmt
        , try whileStmt
        , try returnStmt
        , try breakStmt
        , try continueStmt
        , try operatorDefStmt
        , try typeDefStmt
        , try exprStmt
        ]

    blockStmt :: ParserStmt
    blockStmt = try blockReturnStmt <|> try blockSimpleStmt

    blockSimpleStmt :: ParserStmt
    blockSimpleStmt = do
      stmts <-
        between
          (single TkBraceOpen)
          (single TkBraceClose)
          (many stmt)
      return $ makeComposite stmts

    blockReturnStmt :: ParserStmt
    blockReturnStmt = do
      _ <- single TkBraceOpen

      stmts <- many stmt
      retExpr <- expr

      _ <- single TkBraceClose
      return $ makeComposite (stmts ++ [StBlockReturn retExpr])

    exprStmt :: ParserStmt
    exprStmt = do
      ex <- expr
      _ <- single TkSemiColon
      return $ StExpr ex

    letStmt :: ParserStmt
    letStmt = do
      _ <- single TkLet
      name <- identifier
      _ <- single (TkOp "=")
      value <- expr
      _ <- single TkSemiColon
      return $ StLet name value

    setStmt :: ParserStmt
    setStmt = do
      name <- identifier
      _ <- single (TkOp "=")
      value <- expr
      _ <- single TkSemiColon
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
      _ <- single TkSemiColon
      return $ StReturn value

    breakStmt :: ParserStmt
    breakStmt = StBreak <$ single TkBreak <* single TkSemiColon

    continueStmt :: ParserStmt
    continueStmt = StContinue <$ single TkContinue <* single TkSemiColon

    operatorDefStmt :: ParserStmt
    operatorDefStmt = do
      _ <- single TkOpDef
      ident <- token (\case TkOp x -> Just x; _ -> Nothing) (makeErrSet "operator")
      _ <- single TkParenOpen

      leftIdent <- identifier
      _ <- single TkColon
      leftType <- identifier

      _ <- single TkComma

      rightIdent <- identifier
      _ <- single TkColon
      rightType <- identifier

      _ <- single TkParenClose

      StOperator ident leftIdent leftType rightIdent rightType <$> blockStmt

    typeDefStmt :: ParserStmt
    typeDefStmt = do
      _ <- single TkStruct

      ident <- identifier

      _ <- single TkBraceOpen

      fields <- many field

      _ <- single TkBraceClose

      return $ StType "struct" ident fields
      where
        field = do
          _ <- optional $ single TkPub
          ident <- identifier
          _ <- optional $ single TkColon <* identifier
          _ <- single TkSemiColon
          return ident

    expr :: ParserExpr
    expr =
      choice
        -- \$ map (\x -> x <* notFollowedBy (single TkBraceOpen)) -- hint to normal calls and instantiations
        [ literalNumber
        , literalBool
        , literalString
        , try variable
        , try binary
        , try call
        , try curryCall
        , try instantiation
        , try fieldAccess
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
        variable = ExVariable <$> identifier

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

        curryCall :: ParserExpr
        curryCall = do
          what <- paren
          args <-
            between
              (single TkDollarParenOpen)
              (single TkParenClose)
              (sepBy expr (single TkComma))

          return $ ExCurryCall what args

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
                  identifier
                  (single TkComma)
              )

          ExFnDef args <$> blockStmt

        instantiation :: ParserExpr
        instantiation = do
          what <- paren
          args <-
            between
              (single TkBraceOpen)
              (single TkBraceClose)
              (sepBy expr (single TkComma))

          return $ ExInstantiation what args

        fieldAccess :: ParserExpr
        fieldAccess = do
          what <- paren
          _ <- single TkDot
          ExFieldAccess what <$> identifier