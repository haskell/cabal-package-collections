module Cabal.Collections where

import Distribution.Types.PackageName (PackageName)
import Distribution.Version
import Data.Void
import Data.Text (Text)
import GHC.Exts
import Data.Text.Display

data ShadowMode a
  = ShadowPerName
  | ShadowPerNameVersion
  | Total a
  deriving (Show, Eq, Ord)

data Filter a
  = IsEqual a
  | IsNotEqual a
  deriving (Show, Eq, Ord)

instance IsString a => IsString (Filter a) where
  fromString = IsEqual . fromString

instance Show a => Display (Filter a) where
  displayBuilder = \case
    IsEqual a -> "== " <> displayBuilder (ShowInstance a)
    IsNotEqual a -> "/= " <> displayBuilder (ShowInstance a)

data Collection a
  = Empty
  | Everything
  -- | Not (Collection a)
  | Combine (ShadowMode a) (Collection a) (Collection a)
  | Subtract (Collection a) (Collection a)
  | FilterBySource (Filter SourceName) (Collection a)
  | FilterByName (Filter PackageName) (Collection a)
  -- | FilterByAuthor (Filter AuthorName) (Collection a)
  | SingleVersionConstraint PkgVersionConstraint
  deriving (Show, Eq, Ord)

-- package filters on (source, name) are sane
-- package filters on package information that may be different between versions are *wonky*



instance Display (Collection ()) where
  displayBuilder = \case
    Empty -> "NO"
    Everything -> "YES"
    Combine ShadowPerName l r ->
      "(" <> displayBuilder l <> " `nameUnion` " <> displayBuilder r <> ")"
    Combine ShadowPerNameVersion l r ->
      "(" <> displayBuilder l <> " `nameVersionUnion` " <> displayBuilder r <> ")"
    Combine (Total ()) l r ->
      "(" <> displayBuilder l <> " `union*` " <> displayBuilder r <> ")"
    Subtract l r ->
      "(" <> displayBuilder l <> " \\ " <> displayBuilder r <> ")"
    FilterBySource f c ->
      "(SELECT * FROM " <> displayBuilder c <> " WHERE source " <> displayBuilder f <> ")"
    FilterByName f c ->
      "(SELECT * FROM " <> displayBuilder c <> " WHERE name " <> displayBuilder f <> ")"
    SingleVersionConstraint (PkgVersionConstraint n vr) ->
      displayBuilder (ShowInstance n) <> " { " <> displayBuilder (ShowInstance vr) <> " }"

getConstraints :: Collection () -> PackageName -> [SourceName] -> [(SourceName, [Constraint])]

testPackage :: Collection () -> PackageName -> PackageVersion -> [SourceName] -> [SourceName]
testPackage = _

testPackage' :: Collection () -> [KnownPackage] -> [KnownPackage]
testPackage' = _

simplify :: Collection a -> Collection a
simplify = \case
  Combine _ x Empty -> x
  Combine _ Empty x -> x
  Combine _ Everything _ -> Everything
  Combine _ _ Everything -> Everything

  Empty -> Empty
  Everything -> Everything
  Combine sm c1 c2 ->
    Combine sm (simplify c1) (simplify c2)
  Subtract c1 c2 ->
    Subtract (simplify c1) (simplify c2)
  FilterBySource f c -> FilterBySource f (simplify c)
  FilterByName f c -> FilterByName f (simplify c)
  SingleVersionConstraint vc -> SingleVersionConstraint vc

newtype SourceName = SourceName Text
  deriving (Show, Eq, Ord, IsString)

data PkgVersionConstraint =
  PkgVersionConstraint PackageName VersionRange
  deriving (Show, Eq, Ord)

wat :: Collection ()
wat =
  Combine (Total ())
    (FilterBySource (IsEqual "hackage.haskell.org") c1)
    (FilterBySource (IsEqual "cardano-haskell-packages") c2)
  where
    c1 = SingleVersionConstraint $
      PkgVersionConstraint "base" $ thisVersion $ mkVersion [4, 20]
    c2 = SingleVersionConstraint $
      PkgVersionConstraint "base" $ thisVersion $ mkVersion [4, 21]

unions :: [Collection ()] -> Collection ()
unions = undefined

disallowed :: Collection Void
disallowed = undefined

current :: Collection ()
current = Combine (Total ()) cardanoHaskellPackages hackage

hackage :: Collection a
hackage = FilterBySource "hackage.haskell.org" Everything

cardanoHaskellPackages :: Collection a
cardanoHaskellPackages = FilterBySource "cardano-haskell-packages" Everything

-- stackage LTS-ish: a fixed set of single version constraints
--   per-package
stackageLTS :: Collection ()
stackageLTS = unions
  [ SingleVersionConstraint $ PkgVersionConstraint "base" $
      thisVersion $ mkVersion [4, 20]
  , SingleVersionConstraint $ PkgVersionConstraint "whatever" $
      thisVersion $ mkVersion [1, 0]
  ]

whatever :: Collection ()
whatever = SingleVersionConstraint $
  PkgVersionConstraint "base" v4_21
  where
    v4_21 = thisVersion $ mkVersion [4, 21, 0, 0]

-- stackage plus added packages from hackage
stackagePlusSomeNewerStuff :: Collection ()
stackagePlusSomeNewerStuff =
  Combine ShadowPerNameVersion
    (FilterByName "servant-server" hackage)
    stackageLTS

-- If cardano-haskell-packages provides a package with a given name, never use
--   packages with that name from hackage
preferPackagesInCHAP :: Collection ()
preferPackagesInCHAP =
  Combine ShadowPerName cardanoHaskellPackages hackage

mlabs :: Collection a
mlabs = FilterBySource "mlabs" Everything
mlabsScenario :: Collection a
mlabsScenario =
  Combine ShadowPerName mlabs (Combine ShadowPerName cardanoHaskellPackages hackage)

