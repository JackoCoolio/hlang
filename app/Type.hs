data IntSize
  = S8
  | S16
  | S32
  | S64
  | S128

data Type
  = TInteger Bool IntSize
  | TString
  | TBool
  | TArray Type Int

