module Main where

import System.Environment (getArgs)

import Absyn (Exp(..))
import Canon (basicBlocks, linearize, runCanon, traceSchedule)
import Frame (Frag(..), isFProc, stmOfFrag)
import X64Frame (X64Frame)
import Parser (parseProg)
import Semant (ExpTy(..), runTrans, TransM, transProg)
import Types (approxTy, test)
import Sexp(Sexp(..), toSexp)

main :: IO ()
main = do
  args <- getArgs
  let filePath = case args of
                   f:_ -> f
                   []  -> error "Error: no input file"
  -- Read in source file
  src <- readFile $ filePath

  let p = parseProg filePath src

  -- case result of
  --   Left e  -> putStrLn $ "error" ++ show e
  --   Right r -> putStrLn "success"

  putStrLn $ "Reading file " ++ filePath
  -- putStrLn $ show $ p

  case p of
    Left e  -> putStrLn $ "error" ++ show e
    Right p ->

      -- Here we specify the machine frame module with a type annotation.
      -- let trans = transProg p :: TransM ExpTy X64Frame
      let trans = transProg p :: TransM (Frag X64Frame) X64Frame
          (main, frags, i) = runTrans trans in
        case main of
          Left s -> putStrLn "type error"
          -- Left s -> putStrLn s
          Right main ->
            -- let p'' = ExpTy { expty_exp = expty_exp p',
            --                   expty_ty = approxTy 1 (expty_ty p') } in
              -- putStrLn (show p'') >> putStrLn (show frags)
              -- return ()
            let main_stm = stmOfFrag main
                fun_stms = map stmOfFrag (filter isFProc frags)
                ((main_linear, main_bb, main_flat,
                 funs_linear, fun_bbs, funs_flat), i') = runCanon (
                  do
                    main_linear <- linearize main_stm
                    (main_bb, main_exit) <- basicBlocks main_linear
                    main_flat <- traceSchedule main_bb main_exit
                    funs_linear <- mapM linearize fun_stms
                    fun_bb_exits <- mapM basicBlocks funs_linear
                    let (fun_bbs, fun_exits) = unzip fun_bb_exits
                    funs_flat <- mapM (uncurry traceSchedule) fun_bb_exits
                    return (main_linear, main_bb, main_flat,
                            funs_linear, fun_bbs, funs_flat)
                  ) i
            in do
              putStrLn ("Before linearizing:\n" ++
                        show (toSexp (main_stm : fun_stms)))
              putStrLn ("After linearizing:\n" ++
                        show (toSexp (main_linear : funs_linear)))
              putStrLn ("Basic blocks:\n" ++ show (toSexp (main_bb : fun_bbs)))
              putStrLn ("After trace:\n" ++ show (toSexp (main_flat : funs_flat)))

  -- ty <- test ()
  -- putStrLn (show (approxTy 50 ty))
