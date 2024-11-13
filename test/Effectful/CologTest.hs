{-# OPTIONS_GHC -Wno-orphans #-}
module Effectful.CologTest where
import Data.Text(Text,pack)
--import Test.Tasty
import Test.Tasty.QuickCheck
--import Test.Tasty.HUnit
import Effectful.Colog
import Effectful
import Effectful.Labeled
import Effectful.Provider
import Effectful.Concurrent.Async (runConcurrent, withAsync, wait)
import Effectful.Writer.Static.Shared (runWriter, listen)

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

prop_tellEquals :: Text -> Property
prop_tellEquals t = runPureEff $ do
  (_,t2) <- runLogWriter $ logMsg t
  pure $ t === t2

prop_labeledLogShared :: Text -> Property
prop_labeledLogShared msg = property @Property
  . runPureEff
  . fmap ((msg ===) . snd)
  . runLabeled runLogWriter . labeled $ logMsg msg

prop_providerLogShared :: Text -> Property
prop_providerLogShared msg = property @Property
  . runPureEff
  . fmap ((=== msg) . snd)
  . runWriter @Text
  . runProvider_ (runLogAction) $ provideWith_ (tellLogEff ) $ logMsg @Text msg

prop_tellConcurrent :: Text -> Text -> Property
prop_tellConcurrent m1 m2 = ioProperty @Property 
  . runEff 
  . runConcurrent 
  . fmap ((=== (m1 <> m2)) . snd) 
  . runWriter @Text 
  . runLogAction tellLogEff $ logMsg m1 >> withAsync (logMsg m2) (\a -> wait a >> pure ())
