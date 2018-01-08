module Main where

import System.Environment (getArgs)

import Absyn(Exp(..))
import X64Frame (X64Frame)
import Parser (parseProg)
import Semant (ExpTy(..), runTrans, TransM, transProg)

import Types (approxTy, test)

main :: IO ()
main = do
  args <- getArgs
  let filePath = case args of
                   [f] -> f
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
      let trans = transProg p :: TransM ExpTy X64Frame in
        
        case runTrans trans of
          Left s -> putStrLn s
          Right p' ->
            let p'' = ExpTy { expty_exp = expty_exp p',
                              expty_ty = approxTy 1 (expty_ty p') } in
              -- putStrLn (show p'') >>
              return ()

  -- ty <- test ()
  -- putStrLn (show (approxTy 50 ty))
