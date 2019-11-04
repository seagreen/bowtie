module Bowtie.Untyped.Expr where

import Bowtie.Lib.FreeVars
import Bowtie.Lib.Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

data Expr
  = Var Id
  | Lam Environment Id Expr
  | App Expr Expr

  | Let (HashMap Id Expr) Expr

  | Construct Id [Expr]
  | Case Expr (HashMap Id Match)

  | PrimInt Integer
  | EOp Operation
  deriving (Eq, Show, Generic, NFData)

instance FreeVars Expr where
  freeVars :: Expr -> Set Id
  freeVars topExpr =
    case topExpr of
      Var id ->
        Set.singleton id

      Lam _env id expr ->
        Set.delete id (freeVars expr)

      App e1 e2 ->
        freeVars e1 <> freeVars e2

      Let bindings expr ->
        let
          boundIds :: Set Id
          boundIds =
            Set.fromList (HashMap.keys bindings)
        in
          Set.difference
            (foldMap freeVars bindings <> freeVars expr)
            boundIds

      Construct id exprs ->
        Set.singleton id <> foldMap freeVars exprs -- TODO: remove Set.singleton?

      Case expr bindingsMap ->
        freeVars expr <> foldMap freeVars bindingsMap

      PrimInt _ ->
        mempty

      -- easy to forget there can be free variables in an Op
      EOp op ->
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

data Match
  = Match [Id] Expr
  deriving (Eq, Show, Generic, NFData)

instance FreeVars Match where
  freeVars :: Match -> Set Id
  freeVars (Match bindings expr) =
    Set.difference (freeVars expr) (Set.fromList bindings)

data Operation
  = Compare Expr Expr
  | Plus Expr Expr
  | Multiply Expr Expr
  | ShowInt Expr
  | Panic Expr
  deriving (Eq, Show, Generic, NFData)

newtype Environment
  = Environment { unEnvironment :: HashMap Id Expr }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (NFData)
