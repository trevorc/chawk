module Chawk.Parse
( program
) where


import qualified Chawk.AST as AST

import Text.ParserCombinators.Parsec
import qualified Data.Map as M


data ParseState = ParseState
    { globals   :: M.Map String AST.Name
    , locals    :: M.Map String AST.Name
    }

program :: GenParser Char ParseState AST.Program
program = do
    return $ AST.Program [] M.empty M.empty
