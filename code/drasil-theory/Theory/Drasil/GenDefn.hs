{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables  #-}
module Theory.Drasil.GenDefn (GenDefn, gd, gdNoRefs, gdMK, gdMKNoRefs, gdGetEqMods) where

import Language.Drasil
import Data.Drasil.IdeaDicts (genDefn)
import Theory.Drasil.ModelKinds ( ModelKinds(..), elimMk, setMk, getEqMods )

import Control.Lens (makeLenses, view, lens, (^.), set, Lens', to)

-- | A GenDefn is a ModelKind that may have units
data GenDefn = GD { _mk    :: ModelKinds
                  , gdUnit :: Maybe UnitDefn                  
                  , _deri  :: Maybe Derivation
                  , _ref   :: [Reference]
                  , _sn    :: ShortName
                  , _ra    :: String -- RefAddr
                  , _notes :: [Sentence]
                  }
makeLenses ''GenDefn

lensMk :: forall a. Lens' QDefinition a -> Lens' RelationConcept a -> Lens' GenDefn a
lensMk lq lr = lens g s
    where g :: GenDefn -> a
          g gd_ = elimMk lq lr (gd_ ^. mk)
          s :: GenDefn -> a -> GenDefn
          s gd_ x = set mk (setMk (gd_ ^. mk) lq lr x) gd_

instance HasUID             GenDefn where uid = lensMk uid uid
instance NamedIdea          GenDefn where term = lensMk term term
instance Idea               GenDefn where getA = elimMk (to getA) (to getA) . view mk
instance Definition         GenDefn where defn = lensMk defn defn
instance ConceptDomain      GenDefn where cdom = elimMk (to cdom) (to cdom) . view mk
instance ExprRelat          GenDefn where relat = elimMk (to relat) (to relat) . view mk
instance HasDerivation      GenDefn where derivations = deri
instance HasReference       GenDefn where getReferences = ref
instance HasShortName       GenDefn where shortname = view sn
instance HasRefAddress      GenDefn where getRefAdd = view ra
instance HasAdditionalNotes GenDefn where getNotes = notes
instance MayHaveUnit        GenDefn where getUnit = gdUnit
instance CommonIdea         GenDefn where abrv _ = abrv genDefn
instance Referable          GenDefn where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

gd :: (IsUnit u) => RelationConcept -> Maybe u ->
  Maybe Derivation -> [Reference] -> String -> [Sentence] -> GenDefn
gd r _   _     []   _  = error $ "Source field of " ++ r ^. uid ++ " is empty"
gd r u derivs refs sn_ = 
  GD (OthModel r) (fmap unitWrapper u) derivs refs (shortname' sn_) (prependAbrv genDefn sn_)

gdNoRefs :: (IsUnit u) => RelationConcept -> Maybe u ->
  Maybe Derivation -> String -> [Sentence] -> GenDefn
gdNoRefs r u derivs sn_ = 
  GD (OthModel r) (fmap unitWrapper u) derivs [] (shortname' sn_) (prependAbrv genDefn sn_)

-- TODO: remove above, replace with below... create more constructors for eqModel, deModel, and othModel?

gdMK :: (IsUnit u) => ModelKinds -> Maybe u ->
  Maybe Derivation -> [Reference] -> String -> [Sentence] -> GenDefn
gdMK mkind _   _     []   _  = error $ "Source field of " ++ mkind ^. uid ++ " is empty"
gdMK mkind u derivs refs sn_ = 
  GD mkind (fmap unitWrapper u) derivs refs (shortname' sn_) (prependAbrv genDefn sn_)

gdMKNoRefs :: (IsUnit u) => ModelKinds -> Maybe u ->
  Maybe Derivation -> String -> [Sentence] -> GenDefn
gdMKNoRefs mkind u derivs sn_ = 
  GD mkind (fmap unitWrapper u) derivs [] (shortname' sn_) (prependAbrv genDefn sn_)

gdGetEqMods :: [GenDefn] -> [QDefinition]
gdGetEqMods gdefns = getEqMods (map _mk gdefns)
