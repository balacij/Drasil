module Drasil.BmBnd.Changes.Likely where

import           Language.Drasil
import Data.Drasil.Concepts.Documentation (likeChgDom)

changes :: [ConceptInstance]
changes = [loadingFunction, units, beamConfiguration]

loadingFunction :: ConceptInstance
loadingFunction = cic
    "loadingFunction"
    (S "the loading function is currently fixed as a 3rd order polynomial, but should be entirely from user input")
    "loadingFunction"
    likeChgDom

units :: ConceptInstance
units = cic
    "units"
    (S "the units of the variables may be changed if numbers have poor readability")
    "units"
    likeChgDom

beamConfiguration :: ConceptInstance
beamConfiguration = cic
    "beamConfiguration"
    (S "the loading function is currently fixed as a 3rd order polynomial, but should be entirely from user input")
    "beamConfiguration"
    likeChgDom
