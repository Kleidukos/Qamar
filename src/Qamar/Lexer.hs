module Qamar.Lexer where

import           Text.Megaparsec
import           Text.Megaparsec.Char       (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

consumeSpaces :: Parser ()
consumeSpaces = L.space space1 lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme consumeSpaces

symbol :: Text -> Parser Text
symbol = L.symbol consumeSpaces

-- | 'parens' parses something between parenthesis.

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
