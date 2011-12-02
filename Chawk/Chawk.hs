import Chawk.Parse
import qualified Chawk.AST as AST

import Control.Monad
import Data.Maybe
import IO
import System
import System.Console.GetOpt


class (Monad m) => AwkEnvironment m where

instance AwkEnvironment IO where

interpret :: (AwkEnvironment m) => AST.Program -> m ()
interpret = undefined

data Flag
    = Usage
    | FileInput String

options =
    [ Option ['f'] [] (ReqArg FileInput "FILE") "file to process"
    , Option ['?'] [] (NoArg Usage) "display usage info"
    ]

usage :: IO a
usage = do
    progName <- getProgName
    hPutStr stderr $ usageInfo progName options
    exitFailure

main :: IO ()
main = do
    argv <- getArgs
    let (os, args, errs) = getOpt RequireOrder options argv
    fname <- liftM listToMaybe $ case errs of
        [] -> forM os $ \o -> case o of
            Usage -> usage
            FileInput nm -> return nm
        _ -> do
            mapM_ putStr errs
            usage
    code <- case fname of
        Just fname -> readFile fname
        Nothing -> case args of
            [ text ] -> return text
            _ -> usage
    case parseProgram (fromMaybe "<command-line>" fname) code of
        Right program -> interpret program
        Left error -> do
            hPrint stderr error
            exitFailure
