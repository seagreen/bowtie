module Bowtie.Lib.Id where

import Bowtie.Lib.Prelude
import Control.Monad.State.Class

newtype Id
  = Id Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)
  deriving anyclass (NFData)

-- | Note making this a field of @id@ since then it would
-- be printed every time an @Id@ is shown.
unId :: Id -> Text
unId (Id t) =
  t

genVar :: MonadState Int m => m Id
genVar =
  state (\n -> (Id (show n), n + 1))
