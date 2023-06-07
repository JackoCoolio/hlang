module Diagnostic
where

class DiagnosticName a where
  diagName :: a -> String

