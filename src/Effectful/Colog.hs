module Effectful.Colog (
  Log,
  log,
  runLogAction,
  module Colog.Core.Action,
)
where

import Colog.Core.Action
import Data.Kind
import Effectful
import Effectful.Dispatch.Static
import Effectful.Internal.Env (Env, Relinker (..), consEnv, unconsEnv)
import Effectful.Internal.Utils (inlineBracket)
import Prelude hiding (log)

type Log :: Type -> Effect
data Log msg m a

type LogEff msg es = LogAction (Eff es) msg
type instance DispatchOf (Log msg) = Static NoSideEffects

data instance StaticRep (Log msg) where
  MkLog :: forall localEs msg. !(Env localEs) -> !(LogEff msg localEs) -> StaticRep (Log msg)

unLogEff :: forall es msg. LogEff msg es -> msg -> Env es -> IO ()
unLogEff le message env = unEff (unLogAction le message) env

log :: forall msg es. (Log msg :> es) => msg -> Eff es ()
log message = do
  MkLog env act <- getStaticRep
  unsafeEff_ $ unLogEff act message env

runLogAction :: forall es msg a. LogEff msg es -> Eff (Log msg : es) a -> Eff es a
runLogAction logAct act = unsafeEff $ \env -> do
  inlineBracket
    (consEnv (MkLog env logAct) relinkLog env)
    unconsEnv
    (\es -> unEff act es)

relinkLog :: Relinker StaticRep (Log msg)
relinkLog = Relinker $ \relink (MkLog localEs act) -> do
  newLocalEs <- relink localEs
  pure (MkLog newLocalEs act)
