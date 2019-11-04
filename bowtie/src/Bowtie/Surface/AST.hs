module Bowtie.Surface.AST
  ( module Bowtie.Surface.AST
  , module Bowtie.Type.AST
  ) where

import Bowtie.Lib.FreeVars
import Bowtie.Lib.OrderedMap (OrderedMap)
import Bowtie.Lib.Prelude
import Bowtie.Type.AST

import qualified Bowtie.Lib.OrderedMap as OrderedMap
import qualified Data.Set as Set

data AST = AST
  { astTypes :: OrderedMap Id TypeDeclaration
  , astTerms :: OrderedMap Id (Expr, Type)
  } deriving stock (Eq, Show)

emptyAST :: AST
emptyAST =
  AST OrderedMap.empty OrderedMap.empty

-- | TODO: Specify whether it was a type or term that conflicted.
appendAST :: AST -> AST -> Either Id AST
appendAST (AST a1 b1) (AST a2 b2) = do
  typs <- OrderedMap.append a1 a2
  terms <- OrderedMap.append b1 b2
  pure (AST typs terms)

data Expr
  = Var Id
  | Lam Id (Maybe Type) Expr
  | App Expr Expr

  | Let (OrderedMap Id (Expr, Type)) Expr

  | Construct Id
  | Case Expr [Alt]

  | EInt Integer
  | TextLiteral Text
  deriving (Eq, Show)

data Alt
  = Alt Id [Id] Expr
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
        (freeVars expr <> foldMap freeVars (fmap fst (OrderedMap.elems decls)))
          `Set.difference` Set.fromList (OrderedMap.keys decls) -- NOTE: careful, this part isn't tested

      Construct _ ->
        mempty

      Case e alts ->
        freeVars e <> foldMap freeVars alts

      EInt _ ->
        mempty

      TextLiteral _ ->
        mempty

instance FreeVars Alt where
  freeVars :: Alt -> Set Id
  freeVars (Alt i ids expr) =
    Set.singleton i <> (freeVars expr `Set.difference` Set.fromList ids)
