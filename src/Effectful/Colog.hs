module Effectful.Colog (
  -- * Effect
  Log,
  LogEff,
  logMsg,
  injectLog,

  -- ** Handlers
  runLogAction,
  runLogWriter,

  -- ** 'LogAction's
  tellLogEff,

  -- *** IOE constrained
  byteStringLogEff,
  textLogEff,

  -- * Re-exports
  module Colog.Core.Action,
)
where

import Colog.Core.Action
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Effectful
import Effectful.Dispatch.Static
import Effectful.Internal.Env (Env, Relinker (..), consEnv, unconsEnv)
import Effectful.Internal.Utils (inlineBracket)
import Effectful.Writer.Static.Shared (Writer, runWriter, tell)
import Effectful.FileSystem.IO.ByteString (hPutStr)
import Effectful.FileSystem (FileSystem)
import System.IO (Handle)

-- | Provides the ability to log with an implicit 'LogEff'
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

-- | runs the 'Log' effect using the provided action this is the most general runner
runLogAction :: forall es msg a. LogEff es msg -> Eff (Log msg : es) a -> Eff es a
runLogAction logAct act = unsafeEff $ \env -> do
  inlineBracket
    (consEnv (MkLog env logAct) relinkLog env)
    unconsEnv
    (\es -> unEff act es)

-- | runs the 'Log' effect using 'tellLogEff' and then handles the 'Writer' effect
runLogWriter :: forall es msg a. (Monoid msg) => Eff (Log msg : es) a -> Eff es (a, msg)
runLogWriter = runWriter . runLogAction tellLogEff . inject


-- | logs a message using the implicit 'LogEff'
logMsg :: forall msg es. (Log msg :> es) => msg -> Eff es ()
logMsg message = do
  MkLog env act <- getStaticRep
  unsafeEff_ $ unLogEff act message env

-- | converts a 'LogEff' into another compatible 'LogEff'
injectLog :: forall (xs :: [Effect]) (es :: [Effect]) a. (Subset xs es) => LogEff xs a -> LogEff es a
injectLog = hoistLogAction inject

-- untested
-- | 'LogEff' that delegates to a static shared 'Writer' effect
tellLogEff :: forall es msg. (Writer msg :> es,Monoid msg) => LogEff es msg
tellLogEff = LogAction tell

-- untested
-- | 'LogEff' that writes 'Text' to a 'Handle'
textLogEff :: forall es. (FileSystem :> es) => Handle -> LogEff es Text
textLogEff hdl = LogAction $ hPutStr hdl . encodeUtf8

-- untested
-- | 'LogEff' that writes 'ByteString' to a 'Handle'
byteStringLogEff :: forall es. (FileSystem :> es) => Handle -> LogEff es ByteString
byteStringLogEff hdl = LogAction $ hPutStr hdl
