module Bowtie.Infer.Substitution where

import Bowtie.Lib.Environment
import Bowtie.Lib.Prelude
import Bowtie.Lib.TypeScheme
import Bowtie.Surface.AST

import qualified Data.HashMap.Strict as HashMap

-- tapl 318
newtype Substitution
  = Substitution (HashMap Id Type)
  deriving stock (Eq, Show)
  -- BUG_HISTORY: This means that <> overwrote keys instead of doing
  -- one substitution, then the other
  -- deriving newtype Semigroup
  deriving newtype (Monoid)

-- | Based on the @composeSubst@ function from <doc.md#AlgorithmWStepByStep>.
instance Semigroup Substitution where
  Substitution s1 <> Substitution s2 =
    -- <> is `union`, which in case of conflict uses the first map.
    Substitution (fmap (substType (Substitution s1)) s2 <> s1)

addSubstitution :: Id -> Type -> Substitution -> Substitution
addSubstitution k v (Substitution xs) =
  Substitution (HashMap.insert k v xs)

singleSub :: Id -> Type -> Substitution
singleSub k v =
  Substitution (HashMap.singleton k v)

substExpr :: Substitution -> Expr -> Expr
substExpr sub expr =
  case expr of
    Var _ ->
      expr

    Lam id mType body ->
      Lam
        id
        (fmap (substType sub) mType)
        (substExpr sub body)

    App e1 e2 ->
      App (substExpr sub e1) (substExpr sub e2)

    Let bindings body ->
      Let (fmap (\(a,b) -> (substExpr sub a, substType sub b)) bindings) (substExpr sub body)

    Construct _ ->
      expr

    Case caseExpr alts ->
      Case (substExpr sub caseExpr) (fmap (substAlt sub) alts)

    EInt _ ->
      expr

    EText _ ->
      expr

substAlt :: Substitution -> Alt -> Alt
substAlt sub (Alt id ids expr) =
  Alt id ids (substExpr sub expr)

substEnv :: Substitution -> Environment -> Environment
substEnv sub (Environment env) =
  Environment (fmap (substTypeScheme sub) env)

substType :: Substitution -> Type -> Type
substType sub@(Substitution subst) typ =
  case typ of
    TVariable id ->
      case HashMap.lookup id subst of
        Nothing ->
          typ

        Just new ->
          new

    TConstructor id ->
      case HashMap.lookup id subst of
        Nothing ->
          typ

        Just _ ->
          panic "subst tcon"

    TArrow t1 t2 ->
      TArrow (substType sub t1) (substType sub t2)

    TypeApp t1 t2 ->
      TypeApp (substType sub t1) (substType sub t2)

substTypeScheme :: Substitution -> TypeScheme -> TypeScheme
substTypeScheme (Substitution subHm) (TypeScheme polyVars t) =
  let
    -- Algorithm W Step by Step by Martin G.
    sub2 = Substitution (foldr HashMap.delete subHm polyVars)
  in
    TypeScheme polyVars (substType sub2 t)
