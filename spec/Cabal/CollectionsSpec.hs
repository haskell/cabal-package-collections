module Cabal.CollectionsSpec where

import Test.Hspec
import Test.Hspec.Hedgehog
import Hedgehog

import Cabal.Collections.Gen qualified as Gen

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()
