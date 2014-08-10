{-# LANGUAGE PatternGuards #-}

module IRTS.CodegenEmacsLisp (codegenEmacsLisp) where
import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.CodegenCommon
import Idris.Core.TT
import IRTS.System
import Util.System

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Applicative ((<$>), (<*>), pure)

{- Emacs Lisp compiler backend here. Very simple initial version of
it, at least.

Uses BC in order to allow recursion with high depth, like it is in JS
backend.

ToDo: test/fix everything, add includes, handle errors. -}

codegenEmacsLisp :: CodeGenerator
codegenEmacsLisp ci =
  codegenELisp (simpleDecls ci) (includes ci) [] (outputFile ci) (outputType ci)

codegenELisp
  :: [(Name, SDecl)]
  -> [FilePath]
  -> [String]
  -> FilePath
  -> OutputType
  -> IO ()
codegenELisp definitions includes libs filename outputType = do
  let bytecode = map toBC definitions
  path       <- (++) <$> getDataDir <*> (pure "/elrts/")
  idrRuntime <- readFile $ path ++ "common.el"
  let el       = translateBCs bytecode
  TIO.writeFile filename (T.pack $ idrRuntime ++ (concatMap printElisp $ fixCalls $ map (\(n, c) -> (n, ([], c))) el))
  return ()

translateBCs :: [(Name, [BC])] -> [(String, [Elisp])]
translateBCs [] = []
translateBCs ((n, bc) : rest) = (escapeName n, map translateBC bc) : translateBCs rest

data Elisp = ElRaw String
           | ElCase String [(String, [Elisp])]
           | ElCall String
           | ElSLCall Bool String -- stackless call, second parameter indicates if we're just continuing there
           deriving (Show)

-- traverse given list with a zipper
-- when call occurs, split the stuff, add calls, process the next definition
-- when case occurs, do the same, but process stuff inside the case:
-- just move it outside, making the stack to be like:
-- func-after-case (added when case occurs) -> func-case-1 (addeed in the clause)
-- so, we need a zipper for lists to split them, and a way to add definitions

printElisp :: (String, [Elisp]) -> String
printElisp (name, el) = "(defun " ++ name ++ " (old-base my-old-base)\n" ++
                        concatMap (\x -> "  " ++ printExpr x ++ "\n") el ++
                        ")\n\n"
  where
    printExpr (ElRaw x) = x
    printExpr (ElSLCall False x) = "(idris-call '" ++ x ++ " my-old-base my-old-base)"
    printExpr (ElSLCall True x) = "(idris-call '" ++ x ++ " old-base my-old-base)"
    printExpr (ElCase v cases) =
      "(pcase " ++ v ++
      concatMap (\(pat, code) -> "\n    (" ++ pat ++ " " ++ concatMap printExpr code ++ ")") cases
      ++ ")"
    printExpr x = "(idris-error \"unknown expr: " ++ (show x) ++ "\")"

fixCalls :: [(String, ([Elisp], [Elisp]))] -> [(String, [Elisp])]
-- empty
fixCalls [] = []
-- end of current definition
fixCalls ((name, (prev, [])) : xs) = (name, prev) : fixCalls xs
-- call
fixCalls ((name, (prev, (ElCall cn) : next)) : xs) =
  (name, prev ++ [ElSLCall True nextName, ElSLCall False cn]) : fixCalls ((nextName, ([], next)) : xs)
  where nextName = name ++ "-cont"
fixCalls ((name, (prev, (ElCase cr cl) : next)) : xs) =
  (name, prev ++ [ElSLCall True nextName, ElCase cr (mkCases cl)]) : fixCalls (mkClauses cl ++ (nextName, ([], next)) : xs)
  where
    nextName = name ++ "-cont"
    mkClauses [] = []
    mkClauses ((_, code) : cl) = (clauseName, ([], code)) : mkClauses cl
      where
        clauseName = (name ++ "-case-" ++ (show $ length cl))
    mkCases :: [((String, [Elisp]))] -> [((String, [Elisp]))]
    mkCases [] = []
    mkCases ((pat, _) : cl) = (pat, [ElSLCall True clauseName]) : mkCases cl
      where
        clauseName = (name ++ "-case-" ++ (show $ length cl))
fixCalls ((name, (prev, other : next)) : xs) = fixCalls $ (name, (prev ++ [other], next)) : xs

translateBC :: BC -> Elisp
translateBC bc
  | ASSIGN r1 r2          <- bc = ElRaw $ translateReg r1 (Just $ translateReg r2 Nothing)
  | ASSIGNCONST r c       <- bc = ElRaw $ translateReg r (Just $ show c)
  | UPDATE r1 r2          <- bc = ElRaw $ translateReg r1 (Just $ translateReg r2 Nothing)
  | ADDTOP 0              <- bc = ElRaw $ ""
  | ADDTOP n              <- bc = ElRaw $ "(setq idris-top (+ idris-top " ++ show n ++ "))"
  | NULL r                <- bc = ElRaw $ translateReg r (Just "'()")
  | CALL n                <- bc = ElCall $ escapeName n
  | TAILCALL n            <- bc = ElCall $ escapeName n
  | FOREIGNCALL r _ _ n a <- bc = ElRaw $ translateReg r (Just $ "(" ++ n ++ " " ++
                                                  (intercalate " " $ map (flip translateReg Nothing . snd) a) ++ ")")
  | TOPBASE 0             <- bc = ElRaw $ "(setq idris-top idris-base)"
  | TOPBASE n             <- bc = ElRaw $ "(setq idris-top (+ idris-base " ++ show n ++ "))"
  | BASETOP 0             <- bc = ElRaw $ "(setq idris-base idris-top)"
  | BASETOP n             <- bc = ElRaw $ "(setq idris-base (+ idris-top " ++ show n ++ "))"
  | STOREOLD              <- bc = ElRaw $ "(setq my-old-base idris-base)"
  | SLIDE n               <- bc = ElRaw $ "(idris-slide " ++ show n ++ ")"
  | REBASE                <- bc = ElRaw $ "(setq idris-base old-base)"
  | RESERVE n             <- bc = ElRaw $ "(idris-reserve " ++ show n ++ ")"
  | MKCON r t rs          <- bc = ElRaw $ translateReg r (Just $
                                                  "(list " ++ show t ++ " " ++
                                                  (intercalate " " $ map (flip translateReg Nothing) rs) ++ ")")
  | CASE s r c d          <- bc = ElCase
                                  -- car-safe is here because sometimes we need to use default case
                                  -- when value is not even a list
                                  ("(and (car-safe " ++ (translateReg r Nothing) ++ ") (car " ++
                                   (translateReg r Nothing) ++ "))")
                                  ((map mkCase c) ++ maybe [] (\d' -> [("_", map translateBC d')]) d)
  | CONSTCASE r c d       <- bc = ElCase
                                  (translateReg r Nothing)
                                  ((map mkCase c) ++ maybe [] (\d' -> [("_", map translateBC d')]) d)
  | PROJECT r l a         <- bc = ElRaw $ "(idris-project " ++ translateReg r Nothing ++ " " ++ show l ++ " " ++ show a ++ ")"
  | OP r o a              <- bc = ElRaw $
    translateReg r (Just $ "(" ++ translateOP o ++ " " ++ intercalate " " (map (flip translateReg Nothing) a) ++ ")")
  | ERROR e               <- bc = ElRaw $ "(error " ++ show e ++ ")"
  | otherwise                   = ElRaw $ "//" ++ show bc
  where
    mkCase (c, bc) = (show c, map translateBC bc)

translateOP :: PrimFn -> String
translateOP (LMinus _)   = "-"
translateOP (LPlus _)    = "+"
translateOP LStrConcat   = "concat"
translateOP (LIntStr _)  = "number-to-string"
translateOP n            = "idris-error-undefined " ++ show n

escapeName :: Name -> String
escapeName n = escapeName' (showCG n)
  where
    escapeName' [] = []
    escapeName' (x : xs) = current ++ escapeName' xs
      where current = if x `elem` " ," then ['\\', x] else [x]

prepLVar :: LVar -> String
prepLVar (Loc i) = "loc-" ++ (show i)
prepLVar (Glob n) = "idris-glob-" ++ escapeName n

translateReg :: Reg -> Maybe String -> String
translateReg RVal  Nothing  = "ret"
translateReg Tmp   Nothing  = "tmp"
translateReg (L 0) Nothing  = "(aref idris-stack idris-base)"
translateReg (L n) Nothing  = "(aref idris-stack (+ idris-base " ++ show n ++ "))"
translateReg (T 0) Nothing  = "(aref idris-stack idris-top)"
translateReg (T n) Nothing  = "(aref idris-stack (+ idris-top " ++ show n ++ "))"
translateReg RVal  (Just s) = "(setq ret " ++ s ++ ")"
translateReg Tmp   (Just s) = "(setq tmp " ++ s ++ ")"
translateReg (L 0) (Just s) = "(aset idris-stack idris-base " ++ s ++ ")"
translateReg (L n) (Just s) = "(aset idris-stack (+ idris-base " ++ show n ++ ") " ++ s ++ ")"
translateReg (T 0) (Just s) = "(aset idris-stack idris-top " ++ s ++ ")"
translateReg (T n) (Just s) = "(aset idris-stack (+ idris-top " ++ show n ++ ") " ++ s ++ ")"
