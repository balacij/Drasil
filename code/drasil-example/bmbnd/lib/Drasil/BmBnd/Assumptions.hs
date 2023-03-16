module Drasil.BmBnd.Assumptions where

import Language.Drasil
import Data.Drasil.Concepts.Documentation

assumptions :: [ConceptInstance]
assumptions = [world]

world :: ConceptInstance
world = cic "world" (S "The world model is two-dimensional, observing the instant a load is applied on the beam.") "world" assumpDom
