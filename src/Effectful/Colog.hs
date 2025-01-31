module Effectful.Colog (
  -- * Effect
  Log,
  LogEff,
  logMsg,

  -- ** Handlers
  runLogAction,
  runLogWriter,

  -- ** 'LogAction's
  tellLogEff,

  -- *** 'FileSystem' constrained
  byteStringLogEff,
  textLogEff,

  -- * Re-exports
  module Colog.Core.Action,

  -- * Utilities
  injectLog,
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
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString (hPutStr)
import Effectful.Internal.Env (Env, Relinker (..), consEnv, unconsEnv)
import Effectful.Internal.Utils (inlineBracket)
import Effectful.Writer.Static.Shared (Writer, runWriter, tell)
import System.IO (Handle)

-- | Provides the ability to log with an implicit 'LogEff'
type Log :: Type -> Effect
data Log msg m a

-- | The 'Log' effect can only execute side effects if 'IOE' is visible at the
-- point the effect is introduced via 'runLogAction' or derivatives so it is
-- safe to assume we are pure.
type instance DispatchOf (Log msg) = Static NoSideEffects

-- | The 'StaticRep' of the 'Log' effect stores a 'Env' and a compatible 'LogEff'
data instance StaticRep (Log msg) where
  MkLog :: forall localEs msg. !(Env localEs) -> !(LogEff localEs msg) -> StaticRep (Log msg)

-- | 'LogAction' limited to the 'Eff' monad this is a type synonym so all
-- functions on Colog.Core.Action work on it
type LogEff es msg = LogAction (Eff es) msg

unLogEff :: forall es msg. LogEff es msg -> msg -> Env es -> IO ()
unLogEff le = unEff . unLogAction le

relinkLog :: forall msg. Relinker StaticRep (Log msg)
relinkLog = Relinker $ \relink (MkLog localEs act) -> do
  newLocalEs <- relink localEs
  pure $ MkLog newLocalEs act

-- | logs a message using the implicit 'LogEff'
logMsg :: forall msg es. (Log msg :> es) => msg -> Eff es ()
logMsg message = do
  MkLog env act <- getStaticRep
  unsafeEff_ $ unLogEff act message env

-- | runs the 'Log' effect using the provided action this is the most general runner
runLogAction :: forall es msg a. LogEff es msg -> Eff (Log msg : es) a -> Eff es a
runLogAction logAct act = unsafeEff $ \env ->
  inlineBracket
    (consEnv (MkLog env logAct) relinkLog env)
    unconsEnv
    (\es -> unEff act es)

-- | runs the 'Log' effect using 'tellLogEff' and then handles the 'Writer' effect
runLogWriter :: forall es msg a. (Monoid msg) => Eff (Log msg : es) a -> Eff es (a, msg)
runLogWriter = runWriter . runLogAction tellLogEff . inject

-- | 'LogEff' that delegates to a static shared 'Writer' effect
tellLogEff :: forall es msg. (Writer msg :> es, Monoid msg) => LogEff es msg
tellLogEff = LogAction tell

-- | 'LogEff' that writes 'Text' to a 'Handle' using Utf8 encoding
textLogEff :: forall es. (FileSystem :> es) => Handle -> LogEff es Text
textLogEff hdl = LogAction $ hPutStr hdl . encodeUtf8

-- | 'LogEff' that writes 'ByteString' to a 'Handle'
byteStringLogEff :: forall es. (FileSystem :> es) => Handle -> LogEff es ByteString
byteStringLogEff = LogAction . hPutStr

-- untested

-- | converts a 'LogEff' into another compatible 'LogEff'
injectLog :: forall (xs :: [Effect]) (es :: [Effect]) msg. (Subset xs es) => LogEff xs msg -> LogEff es msg
injectLog = hoistLogAction inject
