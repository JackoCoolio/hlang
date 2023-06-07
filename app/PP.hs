module PP
where

class PrettyPrint a where
  pp :: Int -> a -> String
