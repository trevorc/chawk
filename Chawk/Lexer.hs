module Chawk.Lexer where

import qualified Chawk.AST as AST

import Control.Applicative hiding (many, some, (<|>))
import Control.Monad
import Data.Function
import Data.List
import Data.Ord
import Text.Parsec
import qualified Data.Map as M


whiteSpace :: Parsec String u ()
whiteSpace = void $ many1 $ (many1 $ oneOf " \t\r\f\v") <|> string "\\\n"

lexeme :: Parsec String u a -> Parsec String u a
lexeme p = try $ p <* whiteSpace

symbol :: String -> Parsec String u ()
symbol = void . lexeme . string

word = liftA2 (:) identChar $ many $ identChar <|> digit
    where identChar = char '_' <|> letter

keywords :: [String]
keywords =
    [ "atan2", "index", "match", "sprintf", "substr", "close", "int"
    , "rand", "sqrt", "system", "cos", "length", "sin", "srand"
    , "tolower", "exp", "log", "split", "sub", "toupper", "gsub"
    ]

operator :: String -> Parsec String u ()
operator s = do
  res <- anyOperator
  guard $ res == s

anyOperator :: Parsec String u String
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

keyword :: String -> Parsec String u ()
keyword name = lexeme $ do
                 str <- word
                 guard $ str == name

(<<>>) = between `on` symbol

braces   = "{" <<>> "}"
brackets = "[" <<>> "]"
parens   = "(" <<>> ")"

commaSep :: Parsec String u a -> Parsec String u [a]
commaSep = (`sepBy` symbol ",")

newlineToken :: Parsec String u ()
newlineToken = void $ lexeme $ string "\n"