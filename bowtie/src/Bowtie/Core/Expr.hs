module Bowtie.Core.Expr where

import Bowtie.Lib.FreeVars
import Bowtie.Lib.Prelude
import Bowtie.Type.AST

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

data Expr
  = Var Id
  | Lam Id Type Expr
  | App Expr Expr

  | Let (HashMap Id (Expr, Type)) Expr

  | Construct Id
  | Case Expr [Alt]

  | PrimInt Integer
  | PrimOp Operation
  deriving (Eq, Show)

data Alt
  = Alt Id [Id] Expr
  deriving (Eq, Show)

data Operation
  = Compare Expr Expr
  | Plus Expr Expr -- ^ Only works on PrimInts
  | Multiply Expr Expr -- ^ Only works on PrimInts
  | ShowInt Expr -- ^ Only works on PrimInt
  | Panic Expr -- ^ Only works on Text
  deriving (Eq, Show)

instance FreeVars Expr where
  freeVars :: Expr -> Set Id
  freeVars topExpr =
    case topExpr of
      Var i ->
        Set.singleton i

      Lam i _ e ->
        Set.delete i (freeVars e)

      App e1 e2 ->
        freeVars e1 <> freeVars e2

      Let decls expr ->
           foldMap freeVars (fmap fst (HashMap.elems decls))
        <> freeVars expr `Set.difference` Set.fromList (HashMap.keys decls) -- NOTE: careful, this part isn't tested

      Construct _ ->
        mempty

      Case e alts ->
        freeVars e <> foldMap freeVars alts

      PrimInt _ ->
        mempty

      PrimOp op ->
        case op of
          Compare e1 e2 ->
            freeVars e1 <> freeVars e2

          Plus e1 e2 ->
            freeVars e1 <> freeVars e2

          Multiply e1 e2 ->
            freeVars e1 <> freeVars e2

          ShowInt e ->
            freeVars e

          Panic e ->
            freeVars e

instance FreeVars Alt where
  freeVars :: Alt -> Set Id
  freeVars (Alt i ids expr) =
    Set.singleton i <> freeVars expr `Set.difference` Set.fromList ids
