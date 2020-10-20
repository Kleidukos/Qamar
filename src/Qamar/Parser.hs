module Qamar.Parser where

import           Control.Monad.Combinators.Expr
import qualified Data.Text                      as T
import           Prelude                        hiding (Sum, many)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           Qamar.Lexer

newtype Variable = Variable {getVariable :: Text}
  deriving newtype (Eq, Show)

data Expr =
    QInt  Integer
  | Var   Variable
  | Neg   Expr
  | Subtr Expr Expr
  | Sum   Expr Expr
  | Mult  Expr Expr
  | Let   Expr Expr
  deriving (Eq, Show)

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pInteger
  ]

pInteger :: Parser Expr
pInteger = QInt <$> lexeme L.decimal

pVariable :: Parser Expr
pVariable = Let <$> var
  where
    var :: Parser Expr
    var = Var <$> Variable <$> T.pack <$> parser
    parser :: Parser String
    parser = lexeme
      ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Neg
    , prefix "+" id
    ]
  , [ binary "*" Mult
    ]
  , [ binary "+" Sum
    , binary "-" Subtr
    ]
  , [ binary "=" Let
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
postfix name f = Postfix (f <$ symbol name)
