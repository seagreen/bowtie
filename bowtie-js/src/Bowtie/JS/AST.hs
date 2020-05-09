module Bowtie.JS.AST where

import Bowtie.Lib.Prelude

-- h/t PureScript
data AST
  = Var Id
  | Lam Id AST
  | App AST AST
  | Assignment AST AST
  | Block [AST]
  | Return AST
  | Array [AST]
  | IndexArray AST Natural
  | IfThen AST AST
  | Else AST
  | Throw AST -- Will only be used with JSString
  | Equal AST AST
  | -- | @(() => { " <> ast <> "})()@
    LambdaUnit AST
  | JSInt Integer
  | JSString Text
  | Compare AST AST
  | -- | Only works on Ints
    Plus AST AST
  | -- | Only works on Ints
    Multiply AST AST
  | -- | Only works on Int
    ShowInt AST
  deriving (Eq, Show)
