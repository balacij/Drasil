{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables  #-}
module Theory.Drasil.ModelKinds ( ModelKinds(..), setMk, elimMk, lensMk, getEqMods ) where

import Language.Drasil
import Control.Lens
import Data.Maybe (mapMaybe)

data ModelKinds = EquationalModel QDefinition
                | DEModel RelationConcept
                | OthModel RelationConcept

makeLenses ''ModelKinds

instance HasUID             ModelKinds where uid = lensMk uid uid
-- TODO: it compiles, but it seems it's rather easy to get _anything_ to compile, double check I didnt semantically mess up
instance NamedIdea          ModelKinds where term = lensMk term term
instance Idea               ModelKinds where getA = elimMk (to getA) (to getA)
instance Definition         ModelKinds where defn = lensMk defn defn
instance ConceptDomain      ModelKinds where cdom = elimMk (to cdom) (to cdom)
instance ExprRelat          ModelKinds where relat = elimMk (to relat) (to relat)

elimMk :: Getter QDefinition a -> Getter RelationConcept a -> ModelKinds -> a
elimMk l _ (EquationalModel q) = q ^. l
elimMk _ l (DEModel q)         = q ^. l
elimMk _ l (OthModel q)   = q ^. l

setMk :: ModelKinds -> Setter' QDefinition a -> Setter' RelationConcept a -> a -> ModelKinds
setMk (EquationalModel q) f _ x = EquationalModel $ set f x q
setMk (DEModel q)         _ g x = DEModel $ set g x q
setMk (OthModel q)   _ g x = OthModel $ set g x q

lensMk :: forall a. Lens' QDefinition a -> Lens' RelationConcept a -> Lens' ModelKinds a
lensMk lq lr = lens g s
    where g :: ModelKinds -> a
          g mk = elimMk lq lr mk
          s :: ModelKinds -> a -> ModelKinds
          s mk_ x = setMk mk_ lq lr x

getEqMods :: [ModelKinds] -> [QDefinition]
getEqMods = mapMaybe isEqMod
  where
    isEqMod (EquationalModel f) = Just f
    isEqMod _                   = Nothing
