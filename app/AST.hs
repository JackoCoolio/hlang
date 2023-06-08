module AST where

import qualified Lexer
import PP
import Diagnostic (DiagnosticName, diagName)

nl n = "\n" ++ replicate (n + 2) ' '

data BinOp
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | DotOp

instance PrettyPrint BinOp where
  pp _ = show

instance Show BinOp where
  show AddOp = "+"
  show SubOp = "-"
  show MulOp  = "*"
  show DivOp = "/"
  show DotOp = "."

data UnaryOp
  = NegOp
  deriving (Show)

instance PrettyPrint UnaryOp where
  pp _ NegOp = "-"

newtype Ident = Ident (Lexer.WithPos String) deriving (Show)

instance PrettyPrint Ident where
  pp _ (Ident (Lexer.WithPos tok _)) = tok

data Expr
  = Binary Expr BinOp Expr
  | Unary UnaryOp Expr
  | Group Expr
  | Literal (Lexer.WithPos Lexer.Literal)
  | Field Expr Ident
  | Var Ident
  | IfElse Expr Expr Expr
  | GuardElse Expr (Maybe Ident) Expr Expr
  | FnCallExpr Expr [Expr]
  | IndexExpr Expr Expr
  deriving (Show)

instance DiagnosticName Expr where
  diagName _ = "expression"

instance PrettyPrint Expr where
  pp i (Binary lhs op rhs) = "( " ++ pp i lhs ++ " " ++ pp i op ++ " " ++ pp i rhs ++ " )"
  pp i (Unary op e) = "( " ++ pp i op ++ pp i e ++ " )"
  pp i (Group e) = "( " ++ pp i e ++ " )"
  pp i (Literal (Lexer.WithPos tok _)) = pp i tok 
  pp i (Field e id) = pp i e ++ "." ++ pp i id
  pp i (Var id) = pp i id
  pp i (IfElse a b c) = "if " ++ pp i a ++ " {" ++ nl 2 ++ pp i b ++ nl 0 ++ "} else {" ++ nl 2 ++ pp i c ++ nl 0 ++ "}"
  pp i (GuardElse c Nothing x y) = "guard " ++ pp i c ++ " {" ++ nl 2 ++ pp i x ++ nl 0 ++ "} else {" ++ nl 2 ++ pp i y ++ nl 0 ++ "}"
  pp i (GuardElse c (Just a) x y) = "guard " ++ pp i c ++ " as " ++ pp i a ++ " {" ++ nl 2 ++ pp i x ++ nl 0 ++ "} else {" ++ nl 2 ++ pp i y ++ nl 0 ++ "}"
  pp i (FnCallExpr lhs args) = pp i lhs ++ "(" ++ pp i (head args) ++ ")"
  pp i (IndexExpr lhs index) = pp i lhs ++ "[" ++ pp i index ++ "]"
