{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Effectful.CologTest where
import Data.Monoid(Sum(..))
import Data.Text(Text,pack)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Effectful.Colog
import Effectful
import Effectful.Writer.Dynamic
import Effectful.Labeled
import Effectful.Provider
import Effectful.Concurrent.Async (runConcurrent, withAsync, wait)

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

prop_tellEqualsShared :: Int -> Property
prop_tellEqualsShared i = runPureEff $ do
  (_,Sum i2) <- runWriterShared . runLogAction (tellLogEff @_ @(Sum Int)) $ logMsg $ Sum i
  pure $ i === i2
prop_tellEqualsLocal :: Int -> Property
prop_tellEqualsLocal i = runPureEff $ do
  (_,Sum i2) <- runWriterLocal . runLogAction (tellLogEff @_ @(Sum Int)) $ logMsg $ Sum i
  pure $ i === i2

prop_labeledLogShared :: Text -> Property
prop_labeledLogShared msg = property @Property . runPureEff . fmap fst . runWriterShared @Text . peelLabel . labeled @"" @(Log Text) $ val
  where
    val :: forall es. (Writer Text :> es, Log Text :> es) => Eff es ((),Text)
    val = listen $ logMsg msg
    peelLabel :: forall es. (Writer Text :> es) => Eff (Labeled "" (Log Text) : es) ((),Text) -> Eff es Property
    peelLabel einner = runLabeled (runLogAction tellLogEff) einner >>= \(_,t) -> pure $ t === msg 
prop_labeledLogLocal :: Text -> Property
prop_labeledLogLocal msg = property @Property . runPureEff . fmap fst . runWriterLocal @Text . peelLabel . labeled @"" @(Log Text) $ val
  where
    val :: forall es. (Writer Text :> es, Log Text :> es) => Eff es ((),Text)
    val = listen $ logMsg msg
    peelLabel :: forall es. (Writer Text :> es) => Eff (Labeled "" (Log Text) : es) ((),Text) -> Eff es Property
    peelLabel einner = runLabeled (runLogAction tellLogEff) einner >>= \(_,t) -> pure $ t === msg 

prop_providerLogShared :: Text -> Property
prop_providerLogShared msg = property @Property
  . runPureEff
  . fmap ((=== msg) . snd)
  . runWriterShared @Text
  . runProvider_ (runLogAction) $ provideWith_ (tellLogEff ) $ listen $ logMsg @Text msg
prop_providerLogLocal :: Text -> Property
prop_providerLogLocal msg = property @Property 
  . runPureEff 
  . fmap ((=== msg) . snd) 
  . runWriterLocal @Text
  . runProvider_ (runLogAction) $ provideWith_ (tellLogEff ) $ listen $ logMsg @Text msg

prop_tellConcurrent :: Text -> Text -> Property
prop_tellConcurrent m1 m2 = ioProperty @Property 
  . runEff 
  . runConcurrent 
  . fmap ((=== (m1 <> m2)) . snd) 
  . runWriterShared @Text 
  . runLogAction tellLogEff $ logMsg m1 >> withAsync (logMsg m2) (\a -> wait a >> pure ())
