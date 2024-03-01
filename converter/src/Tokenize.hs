module Tokenize
  ( fruTokenize
  , FruToken (..)
  ) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Scientific (Scientific)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (..)
  , Parsec
  , choice
  , many
  , manyTill
  , satisfy
  , (<?>)
  , (<|>)
  )
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L


opChars :: String
opChars = "=+-*/<>&|"


data FruToken
  = TkNumber Scientific -- primitives
  | TkBool Bool
  | TkString String
  | TkOp String -- operator
  | TkLet -- keywords
  | TkWhile
  | TkReturn
  | TkIf
  | TkElse
  | TkFn
  | TkOpDef
  | TkBreak
  | TkContinue
  | TkStruct
  | TkPub
  | TkBraceOpen -- punctuation
  | TkBraceClose
  | TkParenOpen
  | TkParenClose
  | TkDollarParenOpen
  | TkBracketOpen
  | TkBracketClose
  | TkSemiColon
  | TkColon
  | TkDot
  | TkComma
  | TkIdent String -- identifier
  deriving (Eq, Ord, Show)


type Parser = Parsec Void String


sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")


fruTokenize :: Parser [FruToken]
fruTokenize =
  sc
    *> many
      ( choice
          [ TkBraceOpen <$ char '{' -- punctuation
          , TkBraceClose <$ char '}'
          , TkParenOpen <$ char '('
          , TkParenClose <$ char ')'
          , TkDollarParenOpen <$ char '$' <* char '('
          , TkBracketOpen <$ char '['
          , TkBracketClose <$ char ']'
          , TkSemiColon <$ char ';'
          , TkColon <$ char ':'
          , TkDot <$ char '.'
          , TkComma <$ char ','
          , try (TkNumber <$> literalNumber) -- literals
          , TkBool <$> literalBool
          , TkString <$> literalString
          , TkOp <$> operator -- operator
          , keywordOrIdent -- keyword or identifier
          ]
          <* sc
      )
  where
    literalNumber = L.signed sc L.scientific

    literalBool = (True <$ string "true") <|> (False <$ string "false")

    literalString :: Parser String
    literalString = char '\"' *> manyTill L.charLiteral (char '\"')

    operator = takeWhile1P (Just "operator") (`elem` opChars)

    keywordOrIdent = do
      firstSimbol <- satisfy (\c -> isAlpha c || c == '_') <?> "identifier"
      otherSymbols <- takeWhileP (Just "identifier") (\c -> isAlphaNum c || c == '_')
      return $
        case firstSimbol : otherSymbols of
          "let" -> TkLet
          "while" -> TkWhile
          "return" -> TkReturn
          "if" -> TkIf
          "else" -> TkElse
          "fn" -> TkFn
          "operator" -> TkOpDef
          "break" -> TkBreak
          "continue" -> TkContinue
          "struct" -> TkStruct
          "pub" -> TkPub
          name -> TkIdent name
