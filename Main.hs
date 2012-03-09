module Main where

import Control.Arrow       ((&&&), Kleisli(..), runKleisli)
import System.IO           (hPutStr, hPrint, stderr)
import System.Environment  (getArgs, getProgName)
import System.Exit         (exitFailure)
import System.Console.GetOpt

import Chawk.Interpreter (interpret)
import Chawk.Parser      (parseProgram)


data Flag
    = Usage
    | FileInput String

options :: [OptDescr Flag]
options =
    [ Option ['f'] [] (ReqArg FileInput "FILE") "file to process"
    , Option ['?'] [] (NoArg Usage) "display usage info"
    ]

usage :: IO a
usage = do
  progName <- getProgName
  hPutStr stderr $ usageInfo progName options
  exitFailure

(<&) :: Monad m => (a -> m b) -> a -> m (a, b)
k <& x = runKleisli (Kleisli return &&& Kleisli k) x

parseArgs :: [String] -> IO (FilePath, String)
parseArgs argv =
  case getOpt RequireOrder options argv of
    (_,                _,      err:_) -> hPutStr stderr err >> usage
    ([FileInput path], [],     _)     -> readFile <& path
    ([],               [text], _)     -> return ("<command-line>", text)
    (_,                _,      _)     -> usage

main :: IO ()
main = do
    (path, sourceText) <- parseArgs =<< getArgs
    case parseProgram path sourceText of
        Right program -> interpret program
        Left err -> do
            hPrint stderr err
            exitFailure
