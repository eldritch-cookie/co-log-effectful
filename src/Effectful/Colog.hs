module Effectful.Colog (
  Log,
  log,
  runLogAction,
  module Colog.Core.Action,
)
where

import Colog.Core.Action
import Data.Functor.Classes
import Data.Kind
import Data.Time
import Effectful
import Effectful.Dispatch.Static
import Effectful.Internal.Env (Env, Relinker (..), consEnv, unconsEnv)
import Effectful.Internal.Utils (inlineBracket)
import GHC.Generics (Generic, Generic1)
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
  pure $ MkLog newLocalEs act

-- structured logging
data Severity where
  MkDebugS :: Severity
  MkInfoS :: Severity
  MkNoticeS :: Severity
  MkWarningS :: Severity
  MkErrorS :: Severity
  MkCriticalS :: Severity
  MkAlertS :: Severity
  MkEmergencyS :: Severity
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

data Message a where
  MkMessage ::
    { time :: UTCTime
    , payload :: a
    , severity :: Severity
    } ->
    Message a
  deriving (Eq, Ord, Show, Generic, Functor, Traversable, Foldable, Generic1)

instance Eq1 Message where
  liftEq comp m1 m2 = comp m1.payload m2.payload && m1.time == m2.time && m1.severity == m2.severity
instance Ord1 Message where
  liftCompare comp m1 m2 = case compare m1.time m2.time of
    LT -> LT
    EQ -> case compare m1.severity m2.severity of
      LT -> LT
      EQ -> comp m1.payload m2.payload
      GT -> GT
    GT -> GT
