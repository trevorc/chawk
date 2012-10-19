module Chawk.Lexer where        -- -*- coding: utf-8 -*-

import Control.Applicative hiding (many, some, optional, (<|>))
import Control.Monad
import Data.Function
import Data.List
import Data.Ord
import Text.Parsec

type Parser u = Parsec String u

whiteSpace :: Parser u ()
whiteSpace = void $ many1 $ (many1 $ oneOf " \t\r\f\v") <|> string "\\\n"

lexeme :: Parser u a -> Parser u a
lexeme p = try $ p <* optional whiteSpace

symbol :: String -> Parser u ()
symbol = void . lexeme . string

word :: Parser u String
word = liftA2 (:) identChar $ many $ identChar <|> digit
    where identChar = char '_' <|> letter

integerToken :: Parser u Integer
integerToken = read <$> lexeme (many1 digit)

-- See ISO C Standard § 6.4.4.2 Floating constants
floatingPointToken :: Parser u Double
floatingPointToken = read' <$> lexeme (decimal <|> whole)
  where read' :: String -> Double
        read' = read . ('0':)
        decimal = trailing <|> notTrailing
        whole = (++) <$> many1 digit <*> exp
        trailing = together [ many digit
                            , string "."
                            , many1 digit
                            , option "" exp
                            ]
        notTrailing = together [ many1 digit
                               , string "."
                               , option "" exp
                               ]
        exp = together [ return <$> oneOf "Ee"
                       , option "" (return <$> oneOf "+-")
                       , many1 digit
                       ]
        together ps = concat <$> sequence ps

keywords :: [String]
keywords =
    [ "atan2", "index", "match", "sprintf", "substr", "close", "int"
    , "rand", "sqrt", "system", "cos", "length", "sin", "srand"
    , "tolower", "exp", "log", "split", "sub", "toupper", "gsub"
    ]

operator :: String -> Parser u ()
operator s = do
  res <- anyOperator
  guard $ res == s

anyOperator :: Parser u String
anyOperator = choice $ map string $ operators
    where operators = reverse $ sortBy (comparing length)
                      [ "+=", "-=", "*=", "/=", "%=", "^=", "||",
                        "&&", "==", "<=", ">=", "!=", "++", "++",
                        "--", ">>", "!", ">", "<", "|", "?", ":",
                        "~", "$", "=", ",", ";" ]

identifier :: Parser u String
identifier = lexeme l <?> "identifier"
    where l = do
            str <- word
            guard $ str `notElem` keywords
            return str

keyword :: String -> Parser u ()
keyword name = lexeme l <?> "keyword \"" ++ name ++ "\""
    where l = do
            str <- word
            guard $ str == name

(<<>>) :: String -> String -> Parser u a -> Parser u a
(<<>>) = between `on` symbol

braces :: Parser u a -> Parser u a
braces = "{" <<>> "}"

brackets :: Parser u a -> Parser u a
brackets = "[" <<>> "]"

parens :: Parser u a -> Parser u a
parens = "(" <<>> ")"

commaSep :: Parser u a -> Parser u [a]
commaSep = (`sepBy` symbol ",")

newlineToken :: Parser u ()
newlineToken = void $ many1 $ lexeme $ string "\n"

keywordValue :: String -> a -> Parser u a
keywordValue name value = keyword name *> return value
