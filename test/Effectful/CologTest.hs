{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.CologTest where

import Data.ByteString (ByteString)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding

-- import Test.Tasty

import Data.Knob
import System.IO (BufferMode (..), IOMode (..), hSetBuffering)
import Test.Tasty.QuickCheck

-- import Test.Tasty.HUnit

import Effectful
import Effectful.Colog
import Effectful.Concurrent.Async (runConcurrent, wait, withAsync)
import Effectful.FileSystem (runFileSystem)
import Effectful.Labeled
import Effectful.Provider
import Effectful.Writer.Static.Shared (runWriter)

instance Arbitrary Text where
  arbitrary = fmap fromString arbitrary
instance Arbitrary ByteString where
  arbitrary = fmap fromString arbitrary

prop_tellEquals :: Text -> Property
prop_tellEquals t = runPureEff $ do
  (_, t2) <- runLogWriter $ logMsg t
  pure $ t === t2

prop_labeledLogShared :: Text -> Property
prop_labeledLogShared msg =
  runPureEff
    . fmap ((msg ===) . snd)
    . runLabeled runLogWriter
    . labeled
    $ logMsg msg

prop_providerLogShared :: Text -> Property
prop_providerLogShared msg =
  runPureEff
    . fmap ((=== msg) . snd)
    . runWriter @Text
    . runProvider_ (runLogAction)
    $ provideWith_ (tellLogEff)
    $ logMsg @Text msg

prop_tellConcurrent :: Text -> Text -> Property
prop_tellConcurrent m1 m2 =
  ioProperty @Property
    . runEff
    . runConcurrent
    . fmap ((=== (m1 <> m2)) . snd)
    . runWriter @Text
    . runLogAction tellLogEff
    $ logMsg m1 >> withAsync (logMsg m2) (\a -> wait a >> pure ())

prop_ByteStringLogEff :: ByteString -> Property
prop_ByteStringLogEff bs =
  ioProperty @Property
    . runEff
    . runFileSystem
    $ do
      k <- newKnob mempty
      hdl <- newFileHandle k "prop_ByteStringLogEff" ReadWriteMode
      liftIO $ hSetBuffering hdl NoBuffering
      runLogAction (byteStringLogEff hdl) $ logMsg bs
      bn <- Data.Knob.getContents k
      pure $ bs === bn
prop_TextLogEff :: Text -> Property
prop_TextLogEff bs =
  ioProperty @Property
    . runEff
    . runFileSystem
    $ do
      k <- newKnob mempty
      hdl <- newFileHandle k "prop_ByteStringLogEff" ReadWriteMode
      liftIO $ hSetBuffering hdl NoBuffering
      runLogAction (textLogEff hdl) $ logMsg bs
      bn <- Data.Knob.getContents k
      pure $ bs === decodeUtf8Lenient bn
