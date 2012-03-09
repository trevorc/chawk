module Chawk.Interpreter where

import qualified Chawk.AST as AST

class (Monad m) => AwkEnvironment m where

instance AwkEnvironment IO where

interpret :: (AwkEnvironment m) => AST.Program -> m ()
interpret = undefined
