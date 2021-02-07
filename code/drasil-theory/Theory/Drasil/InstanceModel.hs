{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables  #-}
module Theory.Drasil.InstanceModel
  ( InstanceModel
  , im, imNoDeriv, imNoRefs, imNoDerivNoRefs
  , qwUC, qwC
  ) where

import Language.Drasil
import Theory.Drasil.Classes (HasInputs(inputs), HasOutput(..))
import Data.Drasil.IdeaDicts (inModel)

import Control.Lens (makeLenses, view, lens, (^.), set, Getter, Setter', Lens', to, _1, _2)

type Input = (QuantityDict, Maybe (RealInterval Expr Expr))
type Inputs = [Input]
type Output = QuantityDict
type OutputConstraints = [RealInterval Expr Expr]

newtype ModelKinds = ExistingModel RelationConcept

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
-- elimMk l _ (EquationalModel q) = q ^. l
-- elimMk _ l (DEModel q)         = q ^. l
elimMk _ l (ExistingModel q)        = q ^. l

setMk :: ModelKinds -> Setter' QDefinition a -> Setter' RelationConcept a -> a -> ModelKinds
-- setMk (EquationalModel q) f _ x = EquationalModel $ set f x q
-- setMk (DEModel q)         _ g x = DEModel $ set g x q
setMk (ExistingModel q)        _ g x = ExistingModel $ set g x q

-- TODO: NAME SHADOWING PROBLEM!!!!!!!!
lensMk :: forall a. Lens' QDefinition a -> Lens' RelationConcept a -> Lens' InstanceModel a
lensMk lq lr = lens g s
    where g :: InstanceModel -> a
          g im = elimMk lq lr (im ^. mk)
          s :: InstanceModel -> a -> InstanceModel
          s im x = set mk (setMk (im ^. mk) lq lr x) im

instance HasUID             InstanceModel where uid = lensMk uid uid
instance NamedIdea          InstanceModel where term = lensMk term term
instance Idea               InstanceModel where getA = elimMk (to getA) (to getA) . view mk
instance Definition         InstanceModel where defn = lensMk defn defn
-- instance ConceptDomain      InstanceModel where cdom = cdom . view rc
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
im :: RelationConcept -> Inputs -> Output -> 
  OutputConstraints -> [Reference] -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
im rcon _  _ _  [] _  _  = error $ "Source field of " ++ rcon ^. uid ++ " is empty"
im rcon i o oc r der sn = 
  IM (ExistingModel rcon) i (o, oc) r der (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no derivation
imNoDeriv :: RelationConcept -> Inputs -> Output -> 
  OutputConstraints -> [Reference] -> String -> [Sentence] -> InstanceModel
imNoDeriv rcon _  _ _ [] _  = error $ "Source field of " ++ rcon ^. uid ++ " is empty"
imNoDeriv rcon i o oc r sn =
  IM (ExistingModel rcon) i (o, oc) r Nothing (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no references
imNoRefs :: RelationConcept -> Inputs -> Output -> 
  OutputConstraints -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
imNoRefs rcon i o oc der sn = 
  IM (ExistingModel rcon) i (o, oc) [] der (shortname' sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models; no derivations or references
imNoDerivNoRefs :: RelationConcept -> Inputs -> Output -> 
  OutputConstraints -> String -> [Sentence] -> InstanceModel
imNoDerivNoRefs rcon i o oc sn = 
  IM (ExistingModel rcon) i (o, oc) [] Nothing (shortname' sn) (prependAbrv inModel sn)

-- | For building a quantity with no constraint
qwUC :: (Quantity q, MayHaveUnit q) => q -> Input 
qwUC x = (qw x, Nothing)

-- | For building a quantity with a constraint
qwC :: (Quantity q, MayHaveUnit q) => q -> RealInterval Expr Expr -> Input 
qwC x y = (qw x, Just y)
