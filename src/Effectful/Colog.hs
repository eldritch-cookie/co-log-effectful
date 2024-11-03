module Effectful.Colog (
  Log,
  LogEff,
  runLogAction,
  logMsg,
  module Colog.Core.Action,
)
where

import Colog.Core.Action
import Data.Kind
import Data.Time
import Effectful
import Effectful.Dispatch.Static
import Effectful.Internal.Env (Env, Relinker (..), consEnv, unconsEnv)
import Effectful.Internal.Utils (inlineBracket)
import GHC.Generics (Generic, Generic1)
import GHC.Stack

-- | Effect
type Log :: Type -> Effect
data Log msg m a

type instance DispatchOf (Log msg) = Static NoSideEffects
data instance StaticRep (Log msg) where
  MkLog :: forall localEs msg. !(Env localEs) -> !(LogEff localEs msg) -> StaticRep (Log msg)

-- | 'LogAction' limited to the 'Eff' monad
type LogEff es msg = LogAction (Eff es) msg

unLogEff :: forall es msg. LogEff es msg -> msg -> Env es -> IO ()
unLogEff le = unEff . unLogAction le

relinkLog :: forall msg. Relinker StaticRep (Log msg)
relinkLog = Relinker $ \relink (MkLog localEs act) -> do
  newLocalEs <- relink localEs
  pure $ MkLog newLocalEs act

-- | runs the 'Log' effect using the provided action
runLogAction :: forall es msg a. LogEff es msg -> Eff (Log msg : es) a -> Eff es a
runLogAction logAct act = unsafeEff $ \env -> do
  inlineBracket
    (consEnv (MkLog env logAct) relinkLog env)
    unconsEnv
    (\es -> unEff act es)

-- | logs a message using the implicit 'LogEff'
logMsg :: forall msg es. (Log msg :> es) => msg -> Eff es ()
logMsg message = do
  MkLog env act <- getStaticRep
  unsafeEff_ $ unLogEff act message env

-- | TODO
injectLog :: forall (xs :: [Effect]) (es :: [Effect]) a. (Subset xs es) => LogEff xs a -> LogEff es a
injectLog = hoistLogAction inject

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
    forall a.
    { time :: UTCTime
    , callstack :: CallStack
    , payload :: a
    , severity :: Severity
    } ->
    Message a
  deriving (Show, Generic, Functor, Traversable, Foldable, Generic1)

generateMessage :: forall a m. (MonadIO m) => Severity -> a -> m (Message a)
generateMessage sev payload = do
  time <- liftIO getCurrentTime
  pure $ MkMessage time callStack payload sev
