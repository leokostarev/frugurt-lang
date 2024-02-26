module Tokenize
  ( fruTokenize
  , FruToken (..)
  ) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (takeWhile1P, takeWhileP)
  , Parsec
  , choice
  , many
  , satisfy
  , (<?>)
  , (<|>)
  )
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L


opChars :: String
opChars = "=+-*/<>&|"


data FruToken
  = TkInt Int -- primitives
  | TkBool Bool
  | TkOp String -- operator
  | TkLet -- keywords
  | TkWhile
  | TkReturn
  | TkIf
  | TkElse
  | TkFn
  | TkBreak
  | TkContinue
  | TkBraceOpen -- punctuation
  | TkBraceClose
  | TkParenOpen
  | TkParenClose
  | TkBracketOpen
  | TkBracketClose
  | TkSemiColon
  | TkComma
  | TkIdent String -- identifier
  deriving (Eq, Ord, Show)


type Parser = Parsec Void String


sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")


-- stringLiteral :: Parser String
-- stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

fruTokenize :: Parser [FruToken]
fruTokenize =
  sc
    *> many
      ( choice
          [ TkBraceOpen <$ char '{'
          , TkBraceClose <$ char '}'
          , TkParenOpen <$ char '('
          , TkParenClose <$ char ')'
          , TkBracketOpen <$ char '['
          , TkBracketClose <$ char ']'
          , TkSemiColon <$ char ';'
          , TkComma <$ char ','
          , -- literals
            TkInt <$> literalInt
          , TkBool <$> literalBool
          , -- operator
            TkOp <$> operator
          , -- keyword or identifier
            keywordOrIdent
          ]
          <* sc
      )
  where
    literalInt = L.decimal

    literalBool = (True <$ string "true") <|> (False <$ string "false")

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
          "break" -> TkBreak
          "continue" -> TkContinue
          name -> TkIdent name
