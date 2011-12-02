module Chawk.Parse
( parseProgram
) where


import Chawk.Util
import qualified Chawk.AST as AST

import Control.Applicative hiding (many, some, (<|>))
import Control.Monad
import Data.Either
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Text.ParserCombinators.Parsec.Language as Language
import qualified Text.ParserCombinators.Parsec.Token as P


-- Taken from latest Control.Monad
void :: Functor f => f a -> f ()
void = fmap (const ())

parseEither :: CharParser st a
            -> CharParser st b
            -> CharParser st (Either a b)
l `parseEither` r = (Left <$> l) <|> (Right <$> r)


type AwkParser = CharParser ParseState

data ParseState = ParseState
    { globals   :: M.Map String AST.Name
    , locals    :: M.Map String AST.Name
    }

initialState :: ParseState
initialState = ParseState M.empty M.empty

parseProgram :: String -> String -> Either ParseError AST.Program
parseProgram fname contents = runParser program initialState fname contents

awkLanguage :: P.TokenParser st
awkLanguage = P.makeTokenParser $ Language.emptyDef
    { P.commentLine   = "#"
    , P.nestedComments = False
    , P.opStart = opParser
    , P.opLetter = opParser
    , P.reservedNames =
        [ "atan2", "index", "match", "sprintf", "substr", "close", "int",
          "rand", "sqrt", "system", "cos", "length", "sin", "srand",
          "tolower", "exp", "log", "split", "sub", "toupper", "gsub"
        ]
    , P.reservedOpNames =
        [ "+=", "-=", "*=", "/=", "%=", "^=", "||", "&&", "==",
          "<=", ">=", "!=", "++", "++", "--", ">>", "!",  ">",
          "<",  "|",  "?",  ":",  "~",  "$",  "="
        ]
    } where
    opParser = oneOf ",;+-*%^!><|?:~$=/"

identifier      = P.identifier awkLanguage
reserved        = P.reserved awkLanguage
operator        = P.operator awkLanguage
reservedOp      = P.reservedOp awkLanguage
charLiteral     = P.charLiteral awkLanguage
stringLiteral   = P.stringLiteral awkLanguage
natural         = P.natural awkLanguage
integer         = P.integer awkLanguage
float           = P.float awkLanguage
naturalOrFloat  = P.naturalOrFloat awkLanguage
decimal         = P.decimal awkLanguage
hexadecimal     = P.hexadecimal awkLanguage
octal           = P.octal awkLanguage
symbol          = P.symbol awkLanguage
lexeme          = P.lexeme awkLanguage
whiteSpace      = P.whiteSpace awkLanguage
parens          = P.parens awkLanguage
braces          = P.braces awkLanguage
brackets        = P.brackets awkLanguage
squares         = P.squares awkLanguage
semi            = P.semi awkLanguage
comma           = P.comma awkLanguage
colon           = P.colon awkLanguage
dot             = P.dot awkLanguage
semiSep         = P.semiSep awkLanguage
semiSep1        = P.semiSep1 awkLanguage
commaSep        = P.commaSep awkLanguage
commaSep1       = P.commaSep1 awkLanguage

fromState :: (ParseState -> a) -> AwkParser a
fromState = (<$> getState)

programPart :: AwkParser (Either AST.Function AST.Rule)
programPart = parseFunction `parseEither` parseRule where
    parseFunction = do
        reserved "function"
        undefined
    parseRule = undefined

program :: AwkParser AST.Program
program = do
    (functions, rules) <- liftM partitionEithers $ many programPart
    let functionMap = M.fromList $
            extractFst AST.functionName <$> functions
    glbs <- fromState globals

    return $ AST.Program
        rules
        functionMap
        glbs
