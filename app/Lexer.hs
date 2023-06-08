module Lexer where

import Text.Parsec hiding (token, tokens)
import PP

type Lexer = Parsec String ()

l' :: Lexer a -> String -> Either ParseError a
l' parser = parse parser "repl"

l :: String -> Either ParseError [TokenPos]
l = l' tokens

data Keyword = KwIf | KwElse | KwPub | KwFn | KwGuard | KwAs deriving (Eq)
instance Show Keyword where
  show KwIf = "if"
  show KwElse = "else"
  show KwPub = "pub"
  show KwFn = "fn"
  show KwGuard = "guard"
  show KwAs = "as"

keyword :: Lexer Token
keyword = do
  kw <- try $
    choice $
      map (\kw -> string (show kw) >> return kw)
        [ KwIf,
          KwElse,
          KwPub,
          KwFn,
          KwGuard,
          KwAs
        ]
  return $ Keyword kw

data Token
  = Illegal Char
  | Keyword Keyword
  | Ident String
  | Literal Literal
  | LCurly
  | RCurly
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Plus
  | Minus
  | Star
  | Slash
  | Semi
  | Dot
  deriving (Eq)

instance PrettyPrint Token where
  pp _ (Illegal c) = "<illegal: '" ++ [c] ++ "'>"
  pp _ (Ident s) = s
  pp _ (Keyword kw) = show kw
  pp i (Literal lit) = pp i lit
  pp _ LCurly = "{"
  pp _ RCurly = "}"
  pp _ LParen = "("
  pp _ RParen = ")"
  pp _ LBracket = "["
  pp _ RBracket = "]"
  pp _ Plus = "+"
  pp _ Minus = "-"
  pp _ Star = "*"
  pp _ Slash = "/"
  pp _ Semi = ";"
  pp _ Dot = "."

instance Show Token where
  show (Illegal c) = pp 0 (Illegal c)
  show (Ident _) = "identifier"
  show (Literal lit) = show lit
  show t = "`" ++ pp 0 t ++ "`"

data Literal = Integer' Int | String' String | Boolean Bool deriving (Eq)

instance PrettyPrint Literal where
  pp _ (Integer' x) = show x
  pp _ (String' s) = s
  pp _ (Boolean b)
    | b = "true"
    | not b = "false"

instance Show Literal where
  show (String' _) = "a string"
  show lit = pp 0 lit

illegal :: Lexer Token
illegal = do
  c <- anyChar
  return $ Illegal c

ident :: Lexer Token
ident = do
  pos <- getPosition
  fc <- firstChar
  r <- many rest
  return $ Ident (fc : r)
  where
    firstChar = letter <|> char '_'
    rest = firstChar <|> digit

literal :: Lexer Token
literal = do
  lit <- choice [intLit, strLit, boolLit]
  return $ Literal lit

boolLit :: Lexer Literal
boolLit = choice [try trueLit, try falseLit]
  
trueLit :: Lexer Literal
trueLit = do
  string "true"
  notFollowedBy alphaNum
  return $ Boolean True

falseLit :: Lexer Literal
falseLit = do
  string "false"
  notFollowedBy alphaNum
  return $ Boolean False

intLit :: Lexer Literal
intLit = do
  n <- many1 digit
  return $ Integer' $ read n

strLit :: Lexer Literal
strLit = do
  char '"' -- eat the first double quote
  value <- manyTill anyChar (try (char '"'))
  return $ String' value

lcurly, rcurly, lparen, rparen, lbracket, rbracket, plus, minus, star, slash, semi, dot :: Lexer Token
lcurly = char '{' >> return LCurly
rcurly = char '}' >> return RCurly
lparen = char '(' >> return LParen
rparen = char ')' >> return RParen
plus = char '+' >> return Plus
minus = char '-' >> return Minus
star = char '*' >> return Star
slash = char '/' >> return Slash
semi = char ';' >> return Semi
dot = char '.' >> return Dot
lbracket = char '[' >> return LBracket
rbracket = char ']' >> return RBracket

data WithPos a = WithPos a SourcePos

instance (PrettyPrint a) => PrettyPrint (WithPos a) where
  pp i (WithPos x _) = pp i x

instance Functor WithPos where
  fmap f (WithPos a src) = WithPos (f a) src

instance (Show a) => Show (WithPos a) where
  show (WithPos a src) = show a ++ " @ " ++ show sl ++ ":" ++ show sc
    where
      sl = sourceLine src
      sc = sourceColumn src

withPos :: Lexer a -> Lexer (WithPos a)
withPos lexer = do
  pos <- getPosition
  tok <- lexer
  return $ WithPos tok pos

type TokenPos = WithPos Token

-- First, `choice [ident, literal, ...] <|> illegal` is evaluated. Next, spaces
-- is evaluated, but `<*` leaves the value of `choice <|> illegal`. Then,
-- `withPos` is evaluated with the value of the prior computation, adding the
-- position to create a TokenPos.
token =
  withPos $
    (choice
      [ literal, -- literal must come before ident
        keyword,
        ident,
        lcurly,
        rcurly,
        lparen,
        rparen,
        lbracket,
        rbracket,
        plus,
        minus,
        star,
        slash,
        semi,
        dot
      ] <|> illegal)
      <* spaces

tokens = spaces *> many token <* eof
