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

Uses BC in order to optimize calls, like it is in JS backend.

ToDo: optimize calls, make nice AST and printing with proper spacing,
test/fix everything, add includes, handle errors. -}

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
  TIO.writeFile filename (T.pack $ idrRuntime ++ mkDefuns bytecode)
  return ()

mkDefuns :: [(Name, [BC])] -> String
mkDefuns nb = concatMap mkDefun nb
  where
    mkDefun (name, bc) = "(defun " ++ (escapeName name) ++ " (old-base)\n(let ((my-old-base old-base))\n" ++ intercalate "\n" (map translateBC bc) ++ "))\n\n"

translateBC :: BC -> String
translateBC bc
  | ASSIGN r1 r2          <- bc = translateReg r1 True ++ translateReg r2 False ++ ")"
  | ASSIGNCONST r c       <- bc = translateReg r True ++ show c ++ ")"
  | UPDATE r1 r2          <- bc = translateReg r1 True ++ translateReg r2 False ++ ")"
  | ADDTOP n              <- bc = "(setq idris-top (+ idris-top " ++ show n ++ "))"
  | NULL r                <- bc = translateReg r True ++ "'())"
  | CALL n                <- bc = "(" ++ escapeName n ++ " my-old-base)" -- todo: optimize
  | TAILCALL n            <- bc = "(" ++ escapeName n ++ " my-old-base)" -- todo: optimize
  | FOREIGNCALL r _ _ n a <- bc = translateReg r True ++ " (" ++ n ++ " " ++
                                  (intercalate " " $ map (flip translateReg False . snd) a) ++ "))"
  | TOPBASE n             <- bc = "(setq idris-top (+ idris-base " ++ show n ++ "))"
  | BASETOP n             <- bc = "(setq idris-base (+ idris-top " ++ show n ++ "))"
  | STOREOLD              <- bc = "(setq my-old-base idris-base)"
  | SLIDE n               <- bc = "(idris-slide " ++ show n ++ ")"
  | REBASE                <- bc = "(setq idris-base old-base)"
  | RESERVE n             <- bc = "(idris-reserve " ++ show n ++ ")"
  | MKCON r t rs          <- bc = (translateReg r True) ++
                                  " (list " ++ show t ++ " " ++ (intercalate " " $ map (flip translateReg False) rs) ++ "))"
  | CASE s r c d          <- bc =
    --"case " ++ show s ++ " / " ++ translateReg r False ++ " / " ++ show c ++ " / " ++ show d
    "(pcase " ++ translateReg r False ++ "\n" ++ intercalate "\n" (map varcase c) ++ "\n" ++
    maybe "" (\d' -> "(_ (progn " ++ concatMap translateBC d' ++ "))") d ++
    ")"
  | CONSTCASE r c d       <- bc =
    "(pcase " ++ translateReg r False ++ "\n" ++ intercalate "\n" (map constcase c) ++ "\n" ++
    -- default case
    maybe "" (\d' -> "(_ (progn " ++ concatMap translateBC d' ++ "))") d ++
    ")"
  | PROJECT r l a         <- bc = "(idris-project " ++ translateReg r False ++ " " ++ show l ++ " " ++ show a ++ ")"
  | OP r o a              <- bc =
    translateReg r True ++ "(" ++ translateOP o ++ " " ++ intercalate " " (map (flip translateReg False) a) ++ "))"
  | ERROR e               <- bc = "(error " ++ show e ++ ")"
  | otherwise                   = "//" ++ show bc
  where
    constcase (c, bc) = "(`" ++ show c ++ " (progn " ++ concatMap translateBC bc ++ "))"
    varcase (c, bc) = "(`(" ++ show c ++ " . ,_) (progn " ++ concatMap translateBC bc ++ "))"

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

translateReg :: Reg -> Bool -> String
translateReg reg update
  | RVal <- reg = if update
                  then "(setq ret "
                  else "ret"
  | Tmp  <- reg = if update
                  then "(setq tmp "
                  else "tmp"
  | L n  <- reg = if update
                  then "(aset idris-stack (+ idris-base " ++ show n ++ ") "
                  else "(aref idris-stack (+ idris-base " ++ show n ++ "))"
  | T n  <- reg = if update
                  then "(aset idris-stack (+ idris-top " ++ show n ++ ") "
                  else "(aref idris-stack (+ idris-top " ++ show n ++ "))"
