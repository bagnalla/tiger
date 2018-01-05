-- This file is based on the template from
-- https://github.com/dagit/happy-plus-alex

{
{-# OPTIONS -w  #-}
module Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where
import Prelude hiding (lex)
import Data.Char (chr)
import Control.Monad ( liftM )
import Symtab (Id(..))
import Token (TokenClass(..), unLex)
}
%wrapper "monadUserState"
$digit = 0-9
$alpha = [A-Za-z]
tokens :-

  <0> $white+                       ;
  <0> \"                            { beginString }
  <0, comment> "/*"                 { beginComment }
  <0> "type"                        { lex' TokenType }
  <0> "var"                         { lex' TokenVar }
  <0> "function"                    { lex' TokenFunction }
  <0> "break"                       { lex' TokenBreak }
  <0> "of"                          { lex' TokenOf }
  <0> "end"                         { lex' TokenEnd }
  <0> "in"                          { lex' TokenIn }
  <0> "nil"                         { lex' TokenNil }
  <0> "let"                         { lex' TokenLet }
  <0> "do"                          { lex' TokenDo }
  <0> "to"                          { lex' TokenTo }
  <0> "for"                         { lex' TokenFor }
  <0> "while"                       { lex' TokenWhile }
  <0> "else"                        { lex' TokenElse }
  <0> "then"                        { lex' TokenThen }
  <0> "if"                          { lex' TokenIf }
  <0> "array"                       { lex' TokenArray}
  <0> ":="                          { lex' TokenAssign }
  <0> "|"                           { lex' TokenOr }
  <0> "&"                           { lex' TokenAnd }
  <0> ">="                          { lex' TokenGe }
  <0> ">"                           { lex' TokenGt }
  <0> "<="                          { lex' TokenLe }
  <0> "<"                           { lex' TokenLt }
  <0> "<>"                          { lex' TokenNeq }
  <0> "="                           { lex' TokenEq }
  <0> "/"                           { lex' TokenDivide }
  <0> "*"                           { lex' TokenTimes }
  <0> "+"                           { lex' TokenPlus }
  <0> "-"                           { lex' TokenMinus }
  <0> "."                           { lex' TokenDot }
  <0> "}"                           { lex' TokenRBrace }
  <0> "{"                           { lex' TokenLBrace }
  <0> "]"                           { lex' TokenRBrack }
  <0> "["                           { lex' TokenLBrack }
  <0> ")"                           { lex' TokenRParen }
  <0> "("                           { lex' TokenLParen }
  <0> ";"                           { lex' TokenSemicolon }
  <0> ":"                           { lex' TokenColon }
  <0> ","                           { lex' TokenComma }
  <0> $digit+                       { lex (TokenInt . read) }
  <0> eof                           { lex' TokenEOF }
  <0> $alpha [$alpha $digit \_ \']* { lex (TokenId . Id) }

  <string> \"                       { endString }
  <string> .                        { appendString }
  <string> \\[nt\"]                 { escapeString }

  <comment> "*/"                    { endComment }
  <comment> [.\n]                   ;

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath,
				     commentDepth :: Int,
				     stringBuf :: String }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" 0 ""

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath fp = do
  st <- alexGetUserState
  alexSetUserState (st { filePath = fp })

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

beginString :: AlexAction Token
beginString _ _ = do
  alexSetStartCode string
  alexMonadScan'

appendString :: AlexAction Token
appendString (_,_,_,c:_) _ = do
  s <- alexGetUserState
  alexSetUserState s { stringBuf = c:(stringBuf s) }
  alexMonadScan'

escapeString :: AlexAction Token
escapeString (_,_,_,c:_) _ = do
  let unesc =
        case c of
          'n' -> '\n'
          't' -> '\t'
          '"' -> '"'
  s <- alexGetUserState
  alexSetUserState s { stringBuf = unesc:(stringBuf s) }
  alexMonadScan'

endString :: AlexAction Token
endString (p,_,_,_) _ = do
  s <- alexGetUserState
  let buf = stringBuf s
  alexSetUserState s{ stringBuf = "" }
  alexSetStartCode 0
  return $ Token p (TokenString (reverse buf))

beginComment :: AlexAction Token
beginComment _ _ = do
  s <- alexGetUserState
  alexSetUserState s { commentDepth = (commentDepth s)+1 }
  alexSetStartCode comment
  alexMonadScan'

endComment _ _ = do
  s <- alexGetUserState
  let cd = commentDepth s
  alexSetUserState s { commentDepth = cd-1 }
  let sc' = if cd == 1 then 0 else comment
  alexSetStartCode sc'
  alexMonadScan'

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
