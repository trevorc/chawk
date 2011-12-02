module Chawk.Parse
( program
) where


import Chawk.Util
import qualified Chawk.AST as AST

import Control.Applicative hiding (many, some, (<|>))
import Control.Monad
import Data.Either
import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Data.Map as M


-- Taken from latest Control.Monad
void :: Functor f => f a -> f ()
void = fmap (const ())

type Parser = GenParser Char ParseState

data ParseState = ParseState
    { globals   :: M.Map String AST.Name
    , locals    :: M.Map String AST.Name
    }

fromState :: (ParseState -> a) -> Parser a
fromState g = liftM g getState

lexeme :: Parser a -> Parser a
lexeme p = try $ p <* notFollowedBy alphaNum

keyword :: String -> Parser ()
keyword = void . lexeme . string

identifier :: Parser String
identifier = lexeme (string undefined)

programPart :: Parser (Either AST.Function AST.Rule)
programPart = choice
    [ Left <$> parseFunction
    , Right <$> parseRule
    ] where
    parseFunction = do
        keyword "function"
        undefined
    parseRule = undefined

program :: GenParser Char ParseState AST.Program
program = do
    (functions, rules) <- liftM partitionEithers $ many programPart
    let functionMap = M.fromList $
            extractFst AST.functionName <$> functions
    glbs <- fromState globals

    return $ AST.Program
        rules
        functionMap
        glbs
