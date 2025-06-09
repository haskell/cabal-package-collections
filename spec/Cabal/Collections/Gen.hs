module Cabal.Collections.Gen where

import Data.Text (Text)
import Prelude (IO, pure, (<$>), (<*>))
import Distribution.Types.PackageName
import Distribution.Types.VersionRange
import Distribution.Types.Version
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Cabal.Collections qualified as Collections

shadowMode :: MonadGen m => m a -> m (Collections.ShadowMode a)
shadowMode a = Gen.choice
  [ pure Collections.ShadowPerName
  , pure Collections.ShadowPerNameVersion
  , Collections.Total <$> a
  ]

filter :: MonadGen m => m a -> m (Collections.Filter a)
filter a = Gen.choice
  [ Collections.IsEqual <$> a
  , Collections.IsNotEqual <$> a
  ]

pkgVersionConstraint :: MonadGen m
                     => m PackageName
                     -> m VersionRange
                     -> m Collections.PkgVersionConstraint
pkgVersionConstraint pn vr =
  Collections.PkgVersionConstraint <$> pn
                                   <*> vr

sourceName :: MonadGen m => m Text -> m Collections.SourceName
sourceName a = Collections.SourceName <$> a

collection :: MonadGen m
           => m (Collections.ShadowMode a)
           -> m PackageName
           -> m VersionRange
           -> m Collections.SourceName
           -> m (Collections.Collection a)
collection sm pn vr sn = Gen.recursive Gen.choice
  [ pure Collections.Empty
  , pure Collections.Everything
  , Collections.SingleVersionConstraint <$> pkgVersionConstraint pn vr
  ]
  [ Collections.Combine <$> sm
                        <*> collection sm pn vr sn
                        <*> collection sm pn vr sn
  , Collections.Subtract <$> collection sm pn vr sn
                         <*> collection sm pn vr sn
  , Collections.FilterByName <$> filter pn
                             <*> collection sm pn vr sn
  , Collections.FilterBySource <$> filter sn
                               <*> collection sm pn vr sn
  ]

collection_ :: MonadGen m
            => m a
            -> m (Collections.Collection a)
collection_ a =
  collection (shadowMode a) pn vr (sourceName len8Text)
  where
    vr = thisVersion <$> v
    pn = mkPackageName <$> Gen.string (Range.singleton 8) Gen.alpha
    len8Text = Gen.text (Range.singleton 8) Gen.alpha
    v = mkVersion <$> Gen.list (Range.exponential 1 4) (Gen.integral (Range.exponential 0 100))

genCollection :: IO (Collections.Collection ())
genCollection = Gen.sample (collection_ (pure ()))
