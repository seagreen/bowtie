module Bowtie.Infer.Generalize where

import Bowtie.Infer.Substitution
import Bowtie.Lib.FreeVars
import Bowtie.Lib.Prelude
import Bowtie.Lib.TypeScheme
import Bowtie.Type.AST
import Control.Monad.State.Class
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

generalize :: Set Id -> Type -> TypeScheme
generalize env typ =
  let polyVars :: Set Id
      polyVars =
        freeVars typ `Set.difference` env
   in TypeScheme polyVars typ

instantiate :: forall m. MonadState Int m => TypeScheme -> m Type
instantiate (TypeScheme polyVars typ) = do
  sub <-
    fmap
      (Substitution . HashMap.fromList)
      (traverse pairStaleWithFresh (Set.toList polyVars))
  pure (substType sub typ)
  where
    pairStaleWithFresh :: Id -> m (Id, Type)
    pairStaleWithFresh id = do
      id' <- genVar
      pure (id, TVariable id')
