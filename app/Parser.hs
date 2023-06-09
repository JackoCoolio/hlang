module Parser where

import qualified AST
import Control.Monad (when)
import Data.Functor
import Data.Maybe (isJust, isNothing)
import Lexer hiding (literal, token, keyword, ident)
import Text.Parsec hiding (satisfy)
import Text.Parsec.Pos (updatePosChar)
import PP (PrettyPrint, pp)
import Control.Exception (assert)
import Debug.Trace (trace)
import AST (Stmt)

prog s = (p (do
  out <- expr
  eof
  return out) s)

testFile = "test.ria"

pTest :: Parser a -> IO (Either ParseError a)
pTest parser = do
  text <- readFile testFile
  let (Right tokens) = Lexer.l text
   in
    return $ parse parser testFile tokens

p :: Parser a -> String -> Either ParseError a
p parser s = do
  tokens <- Lexer.l s
  parse parser "repl" tokens

p' :: (PrettyPrint a) => Parser a -> String -> Either ParseError String
p' parser s = do
  out <- p parser s
  return $ pp 0 out

type Parser = Parsec [TokenPos] ()

advance :: SourcePos -> t -> [TokenPos] -> SourcePos
-- if there is a next token, the next position is that token's position
advance _ _ (WithPos _ src : _) = src
-- otherwise, just stay in the same place
advance pos _ [] = pos

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe f x = if f x then Just x else Nothing

satisfyMaybe :: (Token -> Maybe b) -> Parser (WithPos b)
satisfyMaybe f =
  token showToken posFromTok testToken
  where
    showToken = show
    posFromTok (WithPos _ src) = src
    testToken (WithPos tok src) = (\x -> WithPos x src) <$> (f tok)

satisfy :: (Token -> Bool) -> Parser TokenPos
satisfy f =
  token showToken posFromTok testToken
  where
    showToken = show
    posFromTok (WithPos _ src) = src
    testToken = filterMaybe (\(WithPos tok _) -> f tok)

eat :: Token -> Parser TokenPos
eat t = satisfy (==t)

isLiteralMaybe (Lexer.Literal lit) = Just lit
isLiteralMaybe _ = Nothing

isIdentMaybe (Lexer.Ident s) = Just s
isIdentMaybe _ = Nothing

infixBp AST.AddOp = (5, 6)
infixBp AST.SubOp = (5, 6)
infixBp AST.MulOp = (7, 8)
infixBp AST.DivOp = (7, 8)
infixBp AST.DotOp = (13, 14)
infixBp AST.ElseOp = (1, 2)

prefixBp AST.NegOp = 9

data PostfixOp = IndexOp | FnCallOp deriving (Show)
postfixOp :: Parser PostfixOp
postfixOp = choice [eat LParen $> FnCallOp, eat LBracket $> IndexOp]

postfixBp IndexOp = 10
postfixBp FnCallOp = 10

data Delimiter = Paren | Curly | Bracket
delimL Paren = LParen
delimL Curly = LCurly
delimL Bracket = LBracket
delimR Paren = RParen
delimR Curly = RCurly
delimR Bracket = RBracket

grouped :: Delimiter -> Parser a -> Parser a
grouped delim p = do
  eat $ delimL delim
  out <- p
  let right = delimR delim
   in do
    (eat $ right) <?> (show right)
    return out

keyword :: Keyword -> Parser Lexer.TokenPos
keyword kw = satisfy (== Lexer.Keyword kw)

keywordExpect kw = keyword kw <?> show kw

-- | `True` if the given expression doesn't need a trailing semicolon if it is
-- used as a statement.
stmtExprCanSkipSemi :: AST.Expr -> Bool
stmtExprCanSkipSemi (AST.Block _ _) = True
stmtExprCanSkipSemi (AST.Binary leading AST.ElseOp trailing) = stmtExprCanSkipSemi leading && stmtExprCanSkipSemi trailing
stmtExprCanSkipSemi (AST.If _ _) = True -- if exprs end in block exprs
stmtExprCanSkipSemi (AST.Guard _ _ _) = True -- if exprs end in block exprs
stmtExprCanSkipSemi _ = False

exprBlock :: Parser AST.Expr
exprBlock = grouped Curly $ do
  stmts <- many stmt
  trailing <- optionMaybe expr
  return $ AST.Block stmts trailing

ident :: Parser AST.Ident
ident = do
  str <- satisfyMaybe isIdentMaybe
  return $ AST.Ident str

exprIdent :: Parser AST.Expr
exprIdent = do
  str <- satisfyMaybe isIdentMaybe
  return $ AST.Var $ AST.Ident str

exprLiteral :: Parser AST.Expr
exprLiteral = do
  lit <- satisfyMaybe isLiteralMaybe
  return $ AST.Literal lit

exprIf :: Parser AST.Expr
exprIf = do
  keyword KwIf
  clause <- expr
  e <- exprBlock <?> "a block expression"
  return $ AST.If clause e

guardAs :: Parser AST.Ident
guardAs = do
  keyword KwAs
  ident

exprGuard :: Parser AST.Expr
exprGuard = do
  keyword KwGuard
  clause <- expr
  as_ <- optionMaybe $ do
    keyword KwAs
    ident
  e <- exprBlock <?> "a block expression"
  return $ AST.Guard clause as_ e

infixOp :: Parser AST.BinOp
infixOp = do
  choice
    [ eat Plus $> AST.AddOp,
      eat Minus $> AST.SubOp,
      eat Star $> AST.MulOp,
      eat Slash $> AST.DivOp,
      eat Dot $> AST.DotOp,
      keyword KwElse $> AST.ElseOp
    ]

unaryOp :: Parser AST.UnaryOp
unaryOp = choice [eat Minus $> AST.NegOp]

unaryExpr :: Parser AST.Expr
unaryExpr = do
  op <- unaryOp
  subExpr <- exprBp $ prefixBp op
  return $ AST.Unary op subExpr

-- | Traverses the expression AST, transforming each sub-expression using `f`.
-- Note: ignores non-expressions.
traverseExpr f = f . traverseExpr' f
traverseExpr' :: (AST.Expr -> AST.Expr) -> AST.Expr -> AST.Expr
traverseExpr' f (AST.Binary lhs op rhs) = AST.Binary (f $ traverseExpr' f lhs) op (f $ traverseExpr' f rhs)
traverseExpr' f (AST.Unary op e) = AST.Unary op (f $ traverseExpr' f e)
traverseExpr' f (AST.Group e) = AST.Group $ f $ traverseExpr' f e
traverseExpr' f (AST.Field e id) = AST.Field (f $ traverseExpr' f e) id
traverseExpr' f (AST.FnCallExpr e args) = AST.FnCallExpr (f $ traverseExpr' f e) (map (f . (traverseExpr' f)) args)
traverseExpr' f (AST.IndexExpr e i) = AST.IndexExpr (f $ traverseExpr' f e) (f $ traverseExpr' f i)
traverseExpr' _ e = e

exprStructurePass :: AST.Expr -> AST.Expr
exprStructurePass = traverseExpr binToFieldPass

-- | Converts Binary expressions to Field expressions if the binary operator is
-- a dot.
binToFieldPass :: AST.Expr -> AST.Expr
binToFieldPass (AST.Binary l AST.DotOp (AST.Var id)) = AST.Field (binToFieldPass l) id
binToFieldPass e = e

-- | Parses a non-binary expression.
nbExpr :: Parser AST.Expr
nbExpr = choice [exprBlock, exprGuard, exprIf, exprLiteral, unaryExpr, exprIdent]

-- | Parses an expression.
expr :: Parser AST.Expr
expr = exprBp 0 <?> "an expression"

isIdent (AST.Var _) = True
isIdent _ = False

-- | Parses an expression using Pratt parsing.
exprBp :: Int -> Parser AST.Expr
exprBp minBp = do
  lhs <- nbExpr
  e <- exprBpLoop minBp lhs
  return $ exprStructurePass e

exprBpLoop :: Int -> AST.Expr -> Parser AST.Expr
-- exprBpLoop minBp lhs | trace ("eBpL: " ++ show minBp ++ ", " ++ show lhs) False = undefined
exprBpLoop minBp lhs = do
  maybeOp <- optionMaybe $ try $ lookAhead $ choice [Left <$> postfixOp, Right <$> infixOp]
  case maybeOp of
    Nothing -> return lhs
    Just (Left pfOp) ->
      if postfixBp pfOp < minBp
        then return lhs
        else do
          postfixOp
          newLhs <- exprBpLoopPostfix minBp lhs pfOp
          exprBpLoop minBp newLhs
    Just (Right ifOp) -> do
      let (leftBp, rightBp) = infixBp ifOp
       in
        if leftBp < minBp
          then return lhs
          else do
            infixOp
            newLhs <- exprBpLoopInfix minBp rightBp lhs ifOp
            exprBpLoop minBp newLhs

-- exprBpLoopPostfix minBp lhs op | trace ("eBpLPostfix: " ++ show minBp ++ ", " ++ show lhs ++ ", " ++ show op) False = undefined
exprBpLoopPostfix minBp lhs op = do
  case op of
    FnCallOp -> do
      args <- sepEndBy expr (eat Comma)
      eat RParen
      let newLhs = AST.FnCallExpr lhs args
       in
        exprBpLoop minBp newLhs
    IndexOp -> do
      indexExpr <- expr
      eat RBracket
      let newLhs = AST.IndexExpr lhs indexExpr
       in
        exprBpLoop minBp newLhs

exprBpLoopInfix :: Int -> Int -> AST.Expr -> AST.BinOp -> Parser AST.Expr
-- exprBpLoopInfix minBp rightBp lhs op | trace ("eBpLInfix: " ++ show minBp ++ ", " ++ show lhs ++ ", " ++ show op) False = undefined
exprBpLoopInfix minBp rightBp lhs op = do
  rhs <- exprBp rightBp
  let newLhs = AST.Binary lhs op rhs
   in
    exprBpLoop minBp newLhs

stmt :: Parser AST.Stmt
stmt = do
  -- eat any leading semicolons
  many $ eat Semi
  choice [try stmtExpr]

stmtExpr :: Parser AST.Stmt
stmtExpr = do
  e <- expr
  -- if the expression needs a trailing semicolon, eat it, otherwise leave it
  -- for the leading-semicolon handler above
  when (not $ stmtExprCanSkipSemi e) (eat Semi >> return ())
  return $ AST.StmtExpr e
