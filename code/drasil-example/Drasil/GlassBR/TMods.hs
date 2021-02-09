module Drasil.GlassBR.TMods (tMods, pbIsSafe, lrIsSafe) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tm)

import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (isSafeLoad, isSafeProb, pbTolfail, probFail,
  tmDemand, tmLRe)

tMods :: [TheoryModel]
tMods = [pbIsSafe, lrIsSafe]

-- FIXME: This is a hack to see if TheoryModel printing will work. This chunk
-- needs to be updated properly.
-- this is the new function but it still uses the lrIsSafeRC,
-- so basically we have to combine the old function with the new function
-- glass_concept :: [ConceptInstance]
-- glass_concept = []


lrIsSafe :: TheoryModel
lrIsSafe = tm (cw lrIsSafeRC)
   [qw isSafeLoad, qw tmLRe, qw tmDemand] ([] :: [ConceptChunk])
   [lrIsSafeRC] [sy isSafeLoad $= sy tmLRe $> sy tmDemand] [] [makeCite astm2009] 
   "isSafeLoad" [lrIsSafeDesc]

lrIsSafeRC :: QDefinition
lrIsSafeRC = fromEqn' "safetyLoad" (nounPhraseSP "Safety Load")
 lrIsSafeDesc (eqSymb isSafeLoad) Boolean (sy tmLRe $> sy tmDemand)

lrIsSafeDesc :: Sentence
lrIsSafeDesc = tModDesc isSafeLoad

pbIsSafe :: TheoryModel
pbIsSafe = tm (cw pbIsSafeRC) 
  [qw isSafeProb, qw probFail, qw pbTolfail] ([] :: [ConceptChunk])
  [pbIsSafeRC] [sy isSafeProb $= sy probFail $< sy pbTolfail] [] [makeCite astm2009]
  "isSafeProb" [pbIsSafeDesc]

pbIsSafeRC :: QDefinition
pbIsSafeRC = fromEqn' "safetyProbability" (nounPhraseSP "Safety Probability") 
  pbIsSafeDesc (eqSymb isSafeProb) Boolean (sy probFail $< sy pbTolfail)

pbIsSafeDesc :: Sentence
pbIsSafeDesc = tModDesc isSafeProb

tModDesc :: QuantityDict -> Sentence
tModDesc main = S "If" +:+. (ch main `sC` S "the structure is considered safe")
