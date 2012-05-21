module Chawk.Interpreter where

import qualified Chawk.AST as AST

class (Monad m) => AwkEnvironment m where
    puts :: String -> m ()

instance AwkEnvironment IO where
    puts = putStr

interpret :: (AwkEnvironment m) => AST.Program -> m ()
interpret ast = puts $ show ast ++ "\n"
