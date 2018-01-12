module Main where

import System.Environment (getArgs)

import Absyn (Exp(..))
import Canon (linearize, runLinearize)
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
          (main, frags) = runTrans trans in
        case main of
          Left s -> putStrLn "type error"
          -- Left s -> putStrLn s
          Right main ->
            -- let p'' = ExpTy { expty_exp = expty_exp p',
            --                   expty_ty = approxTy 1 (expty_ty p') } in
              -- putStrLn (show p'') >> putStrLn (show frags)
              -- return ()
            let main' = runLinearize (linearize (stmOfFrag main))
                funs = map (\frag -> runLinearize (linearize (stmOfFrag frag)))
                  (filter isFProc frags)
            in
              -- putStrLn (show main) >> putStrLn (show frags)
              putStrLn (show (toSexp (main' : funs)))

  -- ty <- test ()
  -- putStrLn (show (approxTy 50 ty))

-- linearize_frags (frag : rest) =
--   let rest' = linearize_frags rest
--       frag' = case frag of
--         FProc stm frame -> FProc (linearize stm) frame
--         _ -> frag in
--     frag' : rest'
  
