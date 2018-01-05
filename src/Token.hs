module Token (
  TokenClass(..),
  unLex
  ) where

import Symtab (Id(..))

data TokenClass =
  TokenType
  | TokenVar
  | TokenFunction
  | TokenBreak
  | TokenOf
  | TokenEnd
  | TokenIn
  | TokenNil
  | TokenLet
  | TokenDo
  | TokenTo
  | TokenFor
  | TokenWhile
  | TokenElse
  | TokenThen
  | TokenIf
  | TokenArray
  | TokenAssign
  | TokenOr
  | TokenAnd
  | TokenGe
  | TokenGt
  | TokenLe
  | TokenLt
  | TokenNeq
  | TokenEq
  | TokenDivide
  | TokenTimes
  | TokenMinus
  | TokenPlus
  | TokenDot
  | TokenRBrace
  | TokenLBrace
  | TokenRBrack
  | TokenLBrack
  | TokenRParen
  | TokenLParen
  | TokenSemicolon
  | TokenColon
  | TokenComma
  | TokenString String
  | TokenInt Int
  | TokenId Id
  | TokenEOF
    deriving (Eq,Show)

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex TokenType            = "type"
unLex TokenVar             = "var"
unLex TokenFunction        = "function"
unLex TokenBreak           = "break"
unLex TokenOf              = "of"
unLex TokenEnd             = "end"
unLex TokenIn              = "in"
unLex TokenNil             = "nil"
unLex TokenLet             = "let"
unLex TokenDo              = "do"
unLex TokenTo              = "to"
unLex TokenFor             = "for"
unLex TokenWhile           = "while"
unLex TokenElse            = "else"
unLex TokenThen            = "then"
unLex TokenIf              = "if"
unLex TokenArray           = "array"
unLex TokenAssign          = "assign"
unLex TokenOr              = "or"
unLex TokenAnd             = "and"
unLex TokenGe              = ">="
unLex TokenGt              = ">"
unLex TokenLe              = "<="
unLex TokenLt              = "<"
unLex TokenNeq             = "neq"
unLex TokenEq              = "eq"
unLex TokenDivide          = "/"
unLex TokenTimes           = "*"
unLex TokenMinus           = "-"
unLex TokenPlus            = "+"
unLex TokenDot             = "dot"
unLex TokenRBrace          = "}"
unLex TokenLBrace          = "{"
unLex TokenRBrack          = "]"
unLex TokenLBrack          = "["
unLex TokenRParen          = ")"
unLex TokenLParen          = "("
unLex TokenSemicolon       = ";"
unLex TokenColon           = ":"
unLex TokenComma           = ","
unLex (TokenString s)      = s
unLex (TokenInt i)         = show i
unLex (TokenId id)         = show id
unLex TokenEOF             = "<EOF>"
