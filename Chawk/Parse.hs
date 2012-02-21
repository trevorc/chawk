module Chawk.Parse
( parseProgram
) where


import Chawk.Lexer
import Chawk.Util
import qualified Chawk.AST as AST

import Control.Applicative hiding (many, optional, some, (<|>))
import Control.Monad
import Data.Either
import Data.Maybe
import Text.ParserCombinators.Parsec hiding (newline)
import qualified Data.Map as M


data ParseState = ParseState
    { globals   :: M.Map String AST.Name
    , locals    :: M.Map String AST.Name
    }

type AwkParser = CharParser ParseState

parseEither :: CharParser st a
            -> CharParser st b
            -> CharParser st (Either a b)
l `parseEither` r = (Left <$> l) <|> (Right <$> r)


initialState :: ParseState
initialState = ParseState M.empty M.empty

parseProgram :: String -> String -> Either ParseError AST.Program
parseProgram fname contents =
    runParser program initialState fname contents

fromState :: (ParseState -> a) -> AwkParser a
fromState = (<$> getState)

expression :: AwkParser AST.Expression
expression = mzero

expressionStatement :: AwkParser AST.Statement
expressionStatement = AST.ExpressionStatement <$> expression

keywordStatement :: String -> AST.Statement -> AwkParser AST.Statement
keywordStatement s st = keyword s *> pure st

name :: AwkParser AST.Name
name = empty

deleteStatement :: AwkParser AST.Statement
deleteStatement = keyword "delete" *>
                  (AST.Delete
                   <$> name
                   <*> brackets (commaSep expression))

printStatement :: AwkParser AST.Statement
printStatement = empty

simpleStatement :: AwkParser AST.Statement
simpleStatement = deleteStatement
                  <|> printStatement
                  <|> keywordStatement "break" AST.Break
                  <|> keywordStatement "next" AST.Next
                  <|> keywordStatement "continue" AST.Continue
                  <|> expressionStatement

complexStatement :: AwkParser AST.Statement
complexStatement = empty

action :: AwkParser AST.Action
action = braces $ concat <$> sepEndBy statementGroup statementSep
    where statementSep = newline <|> operator ";"
          statementGroup =
              (++) <$> many complexStatement <*>
                       (maybeToList <$> optionMaybe simpleStatement)

parseFunction :: AwkParser AST.Function
parseFunction = do
    keyword "function"
    name <- identifier
    params <- parens $ commaSep $ AST.LocalName <$> identifier
    body <- action
    return $ AST.Function
        { AST.functionName = name
        , AST.parameters = params
        , AST.functionBody = body
        }

parseRule :: AwkParser AST.Rule
parseRule = empty

programPart :: AwkParser (Either AST.Function AST.Rule)
programPart = (parseFunction `parseEither` parseRule) <* many newline

program :: AwkParser AST.Program
program = do
    (functions, rules) <- partitionEithers <$> many programPart
    let functionMap = M.fromList $
            extractFst AST.functionName <$> functions
    glbs <- fromState globals

    return $ AST.Program
        rules
        functionMap
        glbs
