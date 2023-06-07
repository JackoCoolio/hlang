module Main where

import Control.Applicative ((*>), (<$>), (<*), (<*>))
import Data.Either (Either)
import Data.Functor.Identity (Identity)
import Text.Parsec hiding (token, tokens)
import Text.Parsec.String hiding (Parser)
import Text.Parsec.Token (GenTokenParser (lexeme, whiteSpace))

main :: IO ()
main = putStrLn "Hello, Haskell!"
