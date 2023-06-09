module AST where

import qualified Lexer
import PP
import Diagnostic (DiagnosticName, diagName)
import Data.List (intercalate)

-- | Newline with indentation.
nl n = "\n" ++ replicate (2 * n) ' '

data BinOp
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | DotOp
  | ElseOp

instance PrettyPrint BinOp where
  pp _ = show

instance Show BinOp where
  show AddOp = "+"
  show SubOp = "-"
  show MulOp  = "*"
  show DivOp = "/"
  show DotOp = "."
  show ElseOp = "else"

data UnaryOp
  = NegOp
  deriving (Show)

instance PrettyPrint UnaryOp where
  pp _ NegOp = "-"

newtype Ident = Ident (Lexer.WithPos String) deriving (Show)

instance PrettyPrint Ident where
  pp _ (Ident (Lexer.WithPos tok _)) = tok

data Stmt
  = StmtExpr Expr
  | StmtDecl Ident Expr
  deriving (Show)

instance PrettyPrint Stmt where
  pp i (StmtExpr ex) = pp i ex ++ ";"

data Expr
  = Binary Expr BinOp Expr
  | Unary UnaryOp Expr
  | Group Expr
  | Block [Stmt] (Maybe Expr)
  | Literal (Lexer.WithPos Lexer.Literal)
  | Field Expr Ident
  | Var Ident
  | If Expr Expr
  | Guard Expr (Maybe Ident) Expr
  | FnCallExpr Expr [Expr]
  | IndexExpr Expr Expr
  deriving (Show)

instance DiagnosticName Expr where
  diagName _ = "expression"

instance PrettyPrint Expr where
  pp i (Binary lhs op rhs) = "( " ++ pp (i+1) lhs ++ " " ++ pp (i+1) op ++ " " ++ pp (i+1) rhs ++ " )"
  pp i (Unary op e) = "( " ++ pp (i+1) op ++ pp (i+1) e ++ " )"
  pp i (Group e) = "( " ++ pp (i+1) e ++ " )"
  pp i (Block stmts ex)
    = let beforeEx = "{" ++ ppStmts stmts
       in
        (case ex of
          Nothing -> beforeEx
          Just ex -> beforeEx ++ nl (i+1) ++ pp (i+1) ex) ++ nl i ++ "}"
    where
      ppStmt stmt = nl (i+1) ++ pp (i+1) stmt
      ppStmts stmts = intercalate "" (map ppStmt stmts)
  pp i (Literal (Lexer.WithPos tok _)) = pp i tok 
  pp i (Field e id) = pp i e ++ "." ++ pp i id
  pp i (Var id) = pp i id
  pp i (If c e) = "if " ++ pp i c ++ " " ++ pp (i+1) e
  pp i (Guard c Nothing x) = "guard " ++ pp i c ++ " " ++ pp i x
  pp i (Guard c (Just a) x) = "guard " ++ pp i c ++ " as " ++ pp i a ++ " " ++ pp i x
  pp i (FnCallExpr lhs args) = pp i lhs ++ "(" ++ ppArgs args ++ ")"
    where
      ppArgs :: [Expr] -> String
      ppArgs [] = ""
      ppArgs args = intercalate "" (map (\arg -> nl (i+1) ++ pp (i+1) arg ++ ",") args) ++ nl i
  pp i (IndexExpr lhs index) = pp i lhs ++ "[" ++ pp i index ++ "]"
