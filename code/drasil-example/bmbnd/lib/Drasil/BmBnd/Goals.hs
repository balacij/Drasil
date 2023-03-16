module Drasil.BmBnd.Goals where

import           Data.Drasil.Concepts.Documentation
import           Language.Drasil

goals :: [ConceptInstance]
goals = [deflection]

deflection :: ConceptInstance
deflection = cic "deflection"
    (S "Calculate the deflection of the beam under load.")
    "deflection" goalStmtDom
