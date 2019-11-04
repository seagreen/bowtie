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
  | Case AST [Alt]

  | JSInt Integer
  | JSString Text
  | JSOp Operation
  deriving (Eq, Show)

data Alt
  = Alt Id [Id] AST
  deriving (Eq, Show)

data Operation
  = Compare AST AST
  | Plus AST AST -- ^ Only works on Ints
  | Multiply AST AST -- ^ Only works on Ints
  | ShowInt AST -- ^ Only works on Int
  | Panic AST -- ^ Only works on Text
  deriving (Eq, Show)
