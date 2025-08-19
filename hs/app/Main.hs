module Main where
import Data.Char (toLower)
import System.Environment
import System.IO
import HS2Lazy.Syntax
import qualified HS2Lazy.Lexer as L
import qualified HS2Lazy.Parser as P
import qualified HS2Lazy.Static as S
import qualified HS2Lazy.Type as T
import HS2Lazy.Compiler (programToExpr, expandCon, skiCompile)
import HS2Lazy.PatComp (compilePatternMatch)
import HS2Lazy.Optimizer (optimizeExpr)
import HS2Lazy.Builtin (expandBltin)
import HS2Lazy.PPrint (showProgram)
import HS2Lazy.Run

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
