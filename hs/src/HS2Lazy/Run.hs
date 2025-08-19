module HS2Lazy.Run where
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

runIO source = do
  pure $ run source

run source = let (p, as, p', e, ce) = compile source in
  insertNewline 80 $ map toLower $ show e

compile s = (prog, as ++ a, expr2, ski2, ce')
    where (prog, is, ce', as) = S.analyze ce topdecls
          topdecls = P.Decl (P.VarDecl ("@main", [], P.Rhs (P.Var "main") []))
                     : (P.parse (L.lexer "argf" s))
          as' = as ++ T.preludeAssumptions
	  (a, prog') = T.tiProgram ce' as' prog
          prog2 = ([],[is]):prog'
          prog3 = compilePatternMatch prog2
          expr1 = expandCon $ programToExpr prog3
          expr2 = optimizeExpr expr1
          ski1 = skiCompile expr2
	  ski2 = expandBltin ski1
          Just ce = T.addCoreClasses T.initialEnv

insertNewline :: Int -> String -> String
insertNewline n [] = []
insertNewline n s = let (line, s') = splitAt n s
                    in (line ++ '\n' : insertNewline n s')