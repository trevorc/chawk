module Chawk.Parser
( parseProgram
) where


import Chawk.Lexer
import qualified Chawk.AST as AST

import Control.Applicative hiding (many, optional, some)
import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.List (nub)
import Data.Either
import Data.Maybe
import Text.Parsec hiding ((<|>))
import qualified Data.Map as M
import qualified Data.Set as S


type Locals = S.Set String

data ParseState = ParseState
    { stLocals :: Locals }

type AwkParser = Parsec String ParseState

parseEither :: Parsec s u a
            -> Parsec s u b
            -> Parsec s u (Either a b)
l `parseEither` r = (Left <$> l) <|> (Right <$> r)

withLocals :: Locals -> AwkParser a -> AwkParser a
withLocals xs m = modifyState (\st -> st { stLocals = xs })
                  *> m
                  <* modifyState (\st -> st { stLocals = S.empty })

initialState :: ParseState
initialState = ParseState S.empty

parseProgram :: String -> String -> Either ParseError AST.Program
parseProgram fname contents =
    runParser program initialState fname contents

expression :: AwkParser AST.Expression
expression = empty

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
    where statementSep = newlineToken <|> operator ";"
          statementGroup =
              (++) <$> many complexStatement
                   <*> (maybeToList <$> optionMaybe simpleStatement)

parseFunction :: AwkParser AST.Function
parseFunction = do
    keyword "function"
    fnName <- identifier
    params <- parens $ commaSep $ identifier
    guard (params == nub params) <?> "duplicate parameter"
    withLocals (S.fromList params) $ do
                  body <- action
                  return $ AST.Function
                             { AST.functionName = fnName
                             , AST.parameters = params
                             , AST.functionBody = body
                             }

parseRule :: AwkParser AST.Rule
parseRule = empty

programPart :: AwkParser (Either AST.Function AST.Rule)
programPart = (parseFunction `parseEither` parseRule)
              <* many newlineToken

program :: AwkParser AST.Program
program = do
    (functions, rules) <- partitionEithers <$> many programPart
    let functionMap = M.fromList $
            (AST.functionName &&& id) <$> functions
    return $ AST.Program rules functionMap
