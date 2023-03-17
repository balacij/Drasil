module Drasil.BmBnd.Terminology where

import           Language.Drasil

terminology :: [ConceptChunk]
terminology =
  [ slender
  , beam
  , bendingMoment
  , deflection
  , load
  , modulusOfElasticity
  , secondMomentOfArea
  , pinnedSupport
  , rollerSupport
  , simplySupportedBeam
  , shearForce
  , youngsModulus]

type Name = String

type Description = String

mkTerm :: Name -> Description -> ConceptChunk
mkTerm n = dcc
  (n ++ "Term") -- FIXME: Use the proper UID system!
  (nounPhraseSP n)

slender :: ConceptChunk
slender = mkTerm
  "slender"
  "if an object's length to height ratio is at least 10:1, then it is slender"

beam :: ConceptChunk
beam = mkTerm
  "beam"
  "structural component intended to carry loads perpendicularly to their ``long'' axis" -- FIXME: I'm hacking quotation marks!

bendingMoment :: ConceptChunk
bendingMoment = mkTerm
  "bending moment"
  "the internal reaction of a structural element when force is imposed"

deflection :: ConceptChunk
deflection = mkTerm "deflection" "displacement due to deformation under load"

load :: ConceptChunk
load = mkTerm "load" "an applied force"

modulusOfElasticity :: ConceptChunk
modulusOfElasticity = mkTerm
  "modulus of elasticity"
  "measure of lengthwise stiffness of an element as a force is applied lengthwise"

secondMomentOfArea :: ConceptChunk
secondMomentOfArea = mkTerm
  "second moment of area"
  "a measure of how well a cross-section resists bending"

pinnedSupport :: ConceptChunk
pinnedSupport = mkTerm
  "pinned support"
  "a structural support that allows for rotation but neither vertical nor horizontal movement"

rollerSupport :: ConceptChunk
rollerSupport = mkTerm
  "roller support"
  "a structural support that allows for rotation and horizontal movement, but not vertical movement"

simplySupportedBeam :: ConceptChunk
simplySupportedBeam = mkTerm
  "simply supported beam"
  "a beam that has a pinned support on one end and roller support on the other"

shearForce :: ConceptChunk
shearForce = mkTerm "shear force" "force applied perpendicularly to objects"

youngsModulus :: ConceptChunk
youngsModulus =
  mkTerm "Young's modulus" "common synonym for modulus of elasticity"
