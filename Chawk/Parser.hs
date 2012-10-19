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
withLocals xs m = do
  locals <- modifyState (\st -> st { stLocals = xs }) *> m
  modifyState $ \st -> st { stLocals = S.empty }
  return locals

parseProgram :: String -> String -> Either ParseError AST.Program
parseProgram fname contents =
    runParser program (ParseState S.empty) fname contents

lValue :: AwkParser AST.LValue
lValue = directLValue <|> indirectLValue
    where directLValue = AST.DirectLValue <$> name
          indirectLValue = AST.IndirectLValue <$> (operator "$" *> expression)


expression :: AwkParser AST.Expression
expression = functionCall
             <|> fmap AST.LValueReference lValue
    where functionCall = try $ do
                           fn <- identifier
                           args <- parens $ commaSep expression
                           return $ AST.FunctionCall fn args

expressionStatement :: AwkParser AST.Statement
expressionStatement = AST.ExpressionStatement <$> expression

keywordStatement :: String -> AST.Statement -> AwkParser AST.Statement
keywordStatement s st = keyword s *> pure st

name :: AwkParser AST.Name
name = do
  nm <- identifier
  isLocal <- S.member nm . stLocals <$> getState
  return $ if isLocal
             then AST.LocalName nm
             else AST.GlobalName nm

deleteStatement :: AwkParser AST.Statement
deleteStatement = do
  array <- keyword "delete" *> name
  dims <- brackets $ commaSep expression
  return $ AST.Delete array dims

printStatement :: AwkParser AST.Statement
printStatement = empty

simpleStatement :: AwkParser AST.Statement
simpleStatement = deleteStatement
                  <|> doStatement
                  <|> printStatement
                  <|> keywordStatement "break" AST.Break
                  <|> keywordStatement "next" AST.Next
                  <|> keywordStatement "continue" AST.Continue
                  <|> expressionStatement

whileStatement :: AwkParser AST.Statement
whileStatement = do
  cond <- keyword "while" *> parens expression
  body <- optional newlineToken *> complexBody
  return $ AST.Loop False cond body

doStatement :: AwkParser AST.Statement
doStatement = do
  body <- keyword "do" *> optional newlineToken *> action
  cond <- optional newlineToken *> keyword "while" *> parens expression
  return $ AST.Loop True cond body

ifStatement :: AwkParser AST.Statement
ifStatement = do
  cond <- keyword "if" *> parens expression <* optional newlineToken
  thenClause <- complexBody
  elseClause <- optionMaybe $ keyword "else" *> complexBody
  return $ AST.If
             { AST.condition  = cond
             , AST.body       = thenClause
             , AST.alterative = elseClause
             }

forStatement :: AwkParser AST.Statement
forStatement = empty

complexStatement :: AwkParser AST.Statement
-- disjunction based on first lexeme
complexStatement = whileStatement
                   <|> ifStatement
                   <|> forStatement

complexBody :: AwkParser AST.Action
complexBody = action <|> (AST.Action . return <$> statement)

statement :: AwkParser AST.Statement
statement = complexStatement
            <|> (simpleStatement <* terminator)
    where terminator = operator ";"
                       <|> newlineToken
                       <|> try (lookAhead $ operator "}")

action :: AwkParser AST.Action
action = braces $ AST.Action . concat <$> many statementGroup
    where statementGroup = (AST.actionStatements <$> action)
                           <|> many1 statement
                           <|> (emptyStatement *> return [])
          emptyStatement = newlineToken <|> operator ";"

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
parseRule = AST.Rule <$> pattern <*> action
    where pattern = keywordValue "BEGIN" AST.Begin
                    <|> keywordValue "END" AST.End
                    <|> (p =<< commaSep expression)
          p []           = return AST.Always
          p [expr]       = return $ AST.Pattern expr
          p [begin, end] = return $ AST.PatternRange begin end
          p _            = unexpected "Pattern range with too many ,'s"

programPart :: AwkParser (Either AST.Function AST.Rule)
programPart = (parseFunction `parseEither` parseRule)
              <* many newlineToken

program :: AwkParser AST.Program
program = do
  (functions, rules) <- optional whiteSpace *> many newlineToken *>
                        (partitionEithers <$> many programPart)
  eof
  let functionMap = M.fromList $
          (AST.functionName &&& id) <$> functions
  return $ AST.Program rules functionMap
