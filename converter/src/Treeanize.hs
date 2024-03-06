{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Treeanize (toAst, FruExpr (..), FruStmt (..), FruWatch (..), FruField (..)) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust)
import Data.Scientific (Scientific)
import Data.Set (Set, singleton)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, token, try)
  , Parsec
  , between
  , choice
  , many
  , oneOf
  , optional
  , sepBy
  , sepBy1
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
  | ExFunction [String] FruStmt
  | ExInstantiation FruExpr [FruExpr] -- type object * field values
  | ExFieldAccess FruExpr String
  deriving (Show, Eq)


data FruStmt
  = StComposite [FruStmt]
  | StExpr FruExpr
  | StLet String FruExpr
  | StSet [String] FruExpr
  | StIf FruExpr FruStmt FruStmt
  | StWhile FruExpr FruStmt
  | StReturn FruExpr
  | StBlockReturn FruExpr
  | StBreak
  | StContinue
  | StOperator String String String String String FruStmt -- operator ident * left arg ident * left arg type ident * right arg ident * right arg type ident * body
  | StType {getTypeType :: String, getIdent :: String, getFields :: [FruField], getWatches :: [FruWatch]}
  deriving (Show, Eq)


-- helpers

data FruField
  = FruField Bool String (Maybe String)
  deriving (Show, Eq)


data FruWatch
  = FruWatch [String] FruStmt
  deriving (Show, Eq)


data TypeSection
  = FieldsSection [FruField]
  | ConstraintSection [FruWatch]


makeErrSet :: String -> Set (ErrorItem FruToken)
makeErrSet = singleton . Label . NonEmpty.fromList


makeComposite :: [FruStmt] -> FruStmt
makeComposite stmts
  | length stmts == 1 = head stmts
  | otherwise = StComposite stmts


composeType :: FruToken -> String -> [TypeSection] -> FruStmt
composeType typeType ident = foldl applySection basicType
  where
    basicType = StType (typeTypeToStr typeType) ident [] []
    typeTypeToStr = \case
      TkStruct -> "struct"
      _ -> undefined

    applySection :: FruStmt -> TypeSection -> FruStmt
    applySection to@(StType _ _ fields watches) = \case
      FieldsSection fields' -> to{getFields = fields ++ fields'}
      ConstraintSection watches' -> to{getWatches = watches ++ watches'}
    applySection _ = undefined


-- parser

type ParserStmt = Parsec Void [FruToken] FruStmt


type ParserExpr = Parsec Void [FruToken] FruExpr


type ParserExtExpr = Parsec Void [FruToken] (FruExpr -> FruExpr)


identifier :: Parsec Void [FruToken] String
identifier = token (\case TkIdent x -> Just x; _ -> Nothing) (makeErrSet "identifier")


toAst :: ParserStmt
toAst = program
  where
    program :: ParserStmt
    program = do
      stmts <- many stmt <* eof
      return $ makeComposite stmts

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
        , try typeStmt
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
      path <- sepBy1 identifier (single TkDot)
      _ <- single (TkOp "=")
      value <- expr
      _ <- single TkSemiColon
      return $ StSet path value

    ifStmt :: ParserStmt
    ifStmt = do
      _ <- single TkIf
      cond <- expr
      thenBody <- blockSimpleStmt
      return $ StIf cond thenBody (StComposite [])

    ifElseStmt :: ParserStmt
    ifElseStmt = do
      _ <- single TkIf
      cond <- expr
      thenBody <- blockSimpleStmt
      _ <- single TkElse
      StIf cond thenBody <$> (blockSimpleStmt <|> ifElseStmt <|> ifStmt)

    whileStmt :: ParserStmt
    whileStmt = do
      _ <- single TkWhile
      cond <- expr
      StWhile cond <$> blockSimpleStmt

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

    typeStmt :: ParserStmt
    typeStmt = do
      typeType <- oneOf [TkStruct]

      ident <- identifier
      _ <- single TkBraceOpen

      fields <- fieldsSection
      sections <- many section

      _ <- single TkBraceClose

      return $ composeType typeType ident (fields : sections)
      where
        fieldsSection = do
          fields <- many field

          return $ FieldsSection fields
          where
            field = do
              public <- optional (single TkPub)
              ident <- identifier
              fieldType <- optional $ single TkColon *> identifier
              _ <- single TkSemiColon
              return $ FruField (isJust public) ident fieldType

        section = choice [constraintSection]

        constraintSection = do
          _ <- single TkConstraints

          watches <- many watch

          return $ ConstraintSection watches
          where
            watch = do
              _ <- single TkWatch

              fields <-
                between
                  (single TkParenOpen)
                  (single TkParenClose)
                  (sepBy identifier (single TkComma))

              FruWatch fields <$> blockSimpleStmt

    expr = exprConfigurable True
    notBinaryExpr = exprConfigurable False

    exprConfigurable :: Bool -> ParserExpr
    exprConfigurable allowBinary = do
      ex <- simpleExpr
      extensions <- many extensionExpr

      return $ foldl (flip ($)) ex extensions
      where
        simpleExpr :: ParserExpr
        simpleExpr =
          choice
            [ literalNumber
            , literalBool
            , literalString
            , variableExpr
            , parenExpr
            , functionExpr
            ]

        extensionExpr :: ParserExtExpr
        extensionExpr =
          choice
            ( [ try callExpr
              , try curryCallExpr
              , try instantiationExpr
              , try fieldAccessExpr
              ]
                ++ [try binaryExpr | allowBinary]
            )

        literalNumber :: ParserExpr
        literalNumber = ExLiteralNumber <$> token (\case TkNumber x -> Just x; _ -> Nothing) (makeErrSet "number")

        literalBool :: ParserExpr
        literalBool = do
          value <- token (\case TkBool x -> Just x; _ -> Nothing) (makeErrSet "bool")
          return $ ExLiteralBool value

        literalString :: ParserExpr
        literalString = do
          value <- token (\case TkString x -> Just x; _ -> Nothing) (makeErrSet "string")
          return $ ExLiteralString value

        variableExpr :: ParserExpr
        variableExpr = ExVariable <$> identifier

        parenExpr :: ParserExpr
        parenExpr =
          between
            (single TkParenOpen)
            (single TkParenClose)
            expr

        functionExpr :: ParserExpr
        functionExpr = do
          _ <- single TkFn
          args <-
            between
              (single TkParenOpen)
              (single TkParenClose)
              ( sepBy
                  identifier
                  (single TkComma)
              )

          ExFunction args <$> blockStmt

        callExpr :: ParserExtExpr
        callExpr = do
          args <-
            between
              (single TkParenOpen)
              (single TkParenClose)
              (sepBy expr (single TkComma))
          return (`ExCall` args)

        curryCallExpr :: ParserExtExpr
        curryCallExpr = do
          args <-
            between
              (single TkDollarParenOpen)
              (single TkParenClose)
              (sepBy expr (single TkComma))
          return (`ExCurryCall` args)

        binaryExpr :: ParserExtExpr
        binaryExpr = do
          op <- token (\case TkOp x -> Just x; _ -> Nothing) (makeErrSet "operator")
          right <- notBinaryExpr
          return $ \left -> ExBinary op left right

        instantiationExpr :: ParserExtExpr
        instantiationExpr = do
          args <-
            between
              (single TkColonBraceOpen)
              (single TkBraceClose)
              (sepBy expr (single TkComma))
          return (`ExInstantiation` args)

        fieldAccessExpr :: ParserExtExpr
        fieldAccessExpr = do
          _ <- single TkDot
          ident <- identifier
          return (`ExFieldAccess` ident)