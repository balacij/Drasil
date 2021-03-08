{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables  #-}
module Theory.Drasil.InstanceModel
  ( InstanceModel, ModelKinds(..)
  , im, imNoDeriv, imNoRefs, imNoDerivNoRefs
  , qwUC, qwC, getEqMod
  ) where

import Language.Drasil
import Theory.Drasil.Classes (HasInputs(inputs), HasOutput(..))
import Data.Drasil.IdeaDicts (inModel)

import Data.Maybe (mapMaybe)
import Control.Lens (makeLenses, view, lens, (^.), set, Getter, Setter', Lens', to, _1, _2)

type Input = (QuantityDict, Maybe (RealInterval Expr Expr))
type Inputs = [Input]
type Output = QuantityDict
type OutputConstraints = [RealInterval Expr Expr]

data ModelKinds = EquationalModel QDefinition 
                | ExistingModel RelationConcept

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes like derivation, source, etc.
data InstanceModel = IM { _mk :: ModelKinds -- _rc :: RelationConcept
                        , _imInputs :: Inputs
                        , _imOutput :: (Output, OutputConstraints)
                        , _ref :: [Reference]
                        , _deri :: Maybe Derivation
                        ,  lb :: ShortName
                        ,  ra :: String
                        , _notes :: [Sentence]
                        }
makeLenses ''InstanceModel

elimMk :: Getter QDefinition a -> Getter RelationConcept a -> ModelKinds -> a
elimMk l _ (EquationalModel q) = q ^. l
-- elimMk _ l (DEModel q)         = q ^. l
elimMk _ l (ExistingModel q)        = q ^. l

setMk :: ModelKinds -> Setter' QDefinition a -> Setter' RelationConcept a -> a -> ModelKinds
setMk (EquationalModel q) f _ x = EquationalModel $ set f x q
-- setMk (DEModel q)         _ g x = DEModel $ set g x q
setMk (ExistingModel q)        _ g x = ExistingModel $ set g x q

lensMk :: forall a. Lens' QDefinition a -> Lens' RelationConcept a -> Lens' InstanceModel a
lensMk lq lr = lens g s
    where g :: InstanceModel -> a
          g im_ = elimMk lq lr (im_ ^. mk)
          s :: InstanceModel -> a -> InstanceModel
          s im_ x = set mk (setMk (im_ ^. mk) lq lr x) im_

instance HasUID             InstanceModel where uid = lensMk uid uid
instance NamedIdea          InstanceModel where term = lensMk term term
instance Idea               InstanceModel where getA = elimMk (to getA) (to getA) . view mk
instance Definition         InstanceModel where defn = lensMk defn defn
instance ConceptDomain      InstanceModel where cdom = elimMk (to cdom) (to cdom) . view mk
instance ExprRelat          InstanceModel where relat = elimMk (to relat) (to relat) . view mk
instance HasDerivation      InstanceModel where derivations = deri
instance HasReference       InstanceModel where getReferences = ref
instance HasShortName       InstanceModel where shortname = lb
instance HasRefAddress      InstanceModel where getRefAdd = ra
instance HasAdditionalNotes InstanceModel where getNotes = notes
instance Quantity           InstanceModel where
instance CommonIdea         InstanceModel where abrv _ = abrv inModel
instance Referable          InstanceModel where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)
instance HasInputs          InstanceModel where
  inputs          = imInputs
instance HasOutput          InstanceModel where
  output          = imOutput . _1
  out_constraints = imOutput . _2
instance HasSymbol          InstanceModel where symbol = symbol . view output -- ???
instance HasSpace           InstanceModel where typ = output . typ
instance MayHaveUnit        InstanceModel where getUnit = getUnit . view output

-- | Smart constructor for instance models with everything defined
im :: ModelKinds -> Inputs -> Output ->
  OutputConstraints -> [Reference] -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
-- TODO: add "instance HasUID ModelKinds..."
-- im mkind _  _ _  [] _  _  = error $ "Source field of " ++ mkind ^. uid ++ " is empty"
im mkind i o oc r der sn = IM mkind i (o, oc) r der (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no derivation
imNoDeriv :: ModelKinds -> Inputs -> Output ->
  OutputConstraints -> [Reference] -> String -> [Sentence] -> InstanceModel
-- TODO: add "instance HasUID ModelKinds..."
-- imNoDeriv mkind _  _ _ [] _  = error $ "Source field of " ++ mkind ^. uid ++ " is empty"
imNoDeriv mkind i o oc r sn = IM mkind i (o, oc) r Nothing (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no references
imNoRefs :: ModelKinds -> Inputs -> Output ->
  OutputConstraints -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
imNoRefs mkind i o oc der sn = IM mkind i (o, oc) [] der (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no derivations or references
imNoDerivNoRefs :: ModelKinds -> Inputs -> Output ->
  OutputConstraints -> String -> [Sentence] -> InstanceModel
imNoDerivNoRefs mkind i o oc sn = IM mkind i (o, oc) [] Nothing (shortname' sn) (prependAbrv inModel sn)

-- | For building a quantity with no constraint
qwUC :: (Quantity q, MayHaveUnit q) => q -> Input
qwUC x = (qw x, Nothing)

-- | For building a quantity with a constraint
qwC :: (Quantity q, MayHaveUnit q) => q -> RealInterval Expr Expr -> Input
qwC x y = (qw x, Just y)

getEqMod :: [InstanceModel] -> [QDefinition]
getEqMod = mapMaybe (isEqMod . view mk)
  where
    isEqMod (EquationalModel f) = Just f
    isEqMod _                   = Nothing
