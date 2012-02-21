module Chawk.Lexer where

import qualified Chawk.AST as AST

import Control.Applicative hiding (many, some, (<|>))
import Control.Monad
import Data.Function
import Data.List
import Data.Ord
import Text.ParserCombinators.Parsec
import qualified Data.Map as M


whiteSpace :: CharParser st ()
whiteSpace = void $ many1 $ (many1 $ oneOf " \t\r\f\v") <|> string "\\\n"

lexeme :: CharParser st a -> CharParser st a
lexeme p = try $ p <* whiteSpace

symbol :: String -> CharParser st ()
symbol = void . lexeme . string

word = liftA2 (:) identChar $ many $ identChar <|> digit
    where identChar = char '_' <|> letter

keywords :: [String]
keywords =
    [ "atan2", "index", "match", "sprintf", "substr", "close", "int"
    , "rand", "sqrt", "system", "cos", "length", "sin", "srand"
    , "tolower", "exp", "log", "split", "sub", "toupper", "gsub"
    ]

operator :: String -> CharParser st ()
operator s = do
  res <- anyOperator
  guard $ res == s

anyOperator :: CharParser st String
anyOperator = choice $ map string $ operators
    where operators = reverse $ sortBy (comparing length)
                      [ "+=", "-=", "*=", "/=", "%=", "^=", "||",
                        "&&", "==", "<=", ">=", "!=", "++", "++",
                        "--", ">>", "!", ">", "<", "|", "?", ":",
                        "~", "$", "=", ",", ";" ]

identifier = lexeme $ do
               str <- word
               guard $ str `notElem` keywords
               return str

keyword :: String -> CharParser st ()
keyword name = lexeme $ do
                 str <- word
                 guard $ str == name

(<<>>) = between `on` symbol

braces   = "{" <<>> "}"
brackets = "[" <<>> "]"
parens   = "(" <<>> ")"

commaSep :: CharParser st a -> CharParser st [a]
commaSep = (`sepBy` symbol ",")

newline :: CharParser st ()
newline = void $ lexeme $ string "\n"