module Main where
import Data.Char (toLower)
import System.Environment
import System.IO
import Syntax
import qualified Lexer as L
import qualified Parser as P
import qualified Static as S
import qualified Type as T
import Compiler (programToExpr, expandCon, skiCompile)
import PatComp (compilePatternMatch)
import Optimizer (optimizeExpr)
import Builtin (expandBltin)
import PPrint (showProgram)
import Run

main :: IO ()
main = do
  source <- argf
  ski  <- runIO source
  putStrLn $ ski

argf :: IO String
argf = do argv <- getArgs
	  if argv == []
	     then getContents
	     else do conts <- mapM getFileContents argv
		     return (concat conts)

getFileContents :: String -> IO String
getFileContents fname = do handle <- openFile fname ReadMode
			   hGetContents handle
