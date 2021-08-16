{-# LANGUAGE TemplateHaskell #-}
-- | For adding a relation (expression) to a concept.
module Language.Drasil.Chunk.Relation (
  -- * Chunk Type
  RelationConcept,
  -- * Constructors
  makeRC, addRelToCC) where

import Control.Lens (makeLenses, (^.), view, set)

import Language.Drasil.Chunk.Concept (ConceptChunk, dccWDS, cw)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (Express(..), Concept,
  ConceptDomain(..), Definition(..), Idea(..), NamedIdea(..))
import Language.Drasil.ModelExpr.Lang (ModelExpr)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)

-- | For a concept ('ConceptChunk') that also has a 'Relation' attached.
--
-- Ex. We can describe a pendulum arm and then apply an associated equation so that we know its behaviour.
data RelationConcept = RC { _conc :: ConceptChunk
                          , _rel  :: ModelExpr
                          }
makeLenses ''RelationConcept

-- | Finds the 'UID' of the 'ConceptChunk' used to make the 'RelationConcept'.
instance HasUID        RelationConcept where uid = conc . uid
-- | Equal if 'UID's are equal.
instance Eq            RelationConcept where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the term ('NP') of the 'ConceptChunk' used to make the 'RelationConcept'.
instance NamedIdea     RelationConcept where term = conc . term
-- | Finds the idea contained in the 'ConceptChunk' used to make the 'RelationConcept'.
instance Idea          RelationConcept where getA = getA . view conc
-- | Finds the definition contained in the 'ConceptChunk' used to make the 'RelationConcept'.
instance Definition    RelationConcept where defn = conc . defn
-- | Finds the domain of the 'ConceptChunk' used to make the 'RelationConcept'.
instance ConceptDomain RelationConcept where cdom = cdom . view conc
-- | Convert the 'RelationConcept' into the model expression language.
instance Express       RelationConcept where express = (^. rel)

-- | Create a 'RelationConcept' from a given 'UID', term ('NP'), definition ('Sentence'), and 'Relation'.
makeRC :: Express e => UID -> NP -> Sentence -> e -> RelationConcept
makeRC rID rTerm rDefn = RC (dccWDS rID rTerm rDefn) . express

-- | Create a new 'RelationConcept' from an old 'Concept'. Takes a 'Concept', new 'UID' and relation.
addRelToCC :: (Express e, Concept c) => c -> UID -> e -> RelationConcept
addRelToCC c rID = RC (set uid rID (cw c)) . express
