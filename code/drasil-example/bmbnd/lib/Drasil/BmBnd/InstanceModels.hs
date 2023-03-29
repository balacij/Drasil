module Drasil.BmBnd.InstanceModels where

import           Theory.Drasil
import           Drasil.BmBnd.Quantities
import           Language.Drasil
import qualified Drasil.BmBnd.Assumptions as As
import Data.Drasil.SI_Units (metre)
import Control.Lens
import Language.Drasil.Development (showUID)

models :: [InstanceModel]
models = [deflection]

y_BCC :: ConstrConcept
y_BCC = cuc' (showUID y_B)
  (nounPhraseSP "temperature of the water")
  "the average kinetic energy of the particles within the water" 
  (eqSymb y_B) metre (Vect Real)
  [] (exactDbl 0)

deflectionDM :: DifferentialModel 
deflectionDM = 
  makeASystemDE
    x
    y_BCC
    coeffs
    unknowns
    constants
    "deflectionIM" 
    (nounPhraseSP "deflection as an instance model") 
    (y_B ^. defn)
    where coeffs = [[sy e_B `mulRe` sy i_B, exactDbl 0]]
          unknowns = [4, 0] -- FIXME: if I don't put a 0, then the program runs out of memory!
          constants = [apply1 w_B x]

deflection :: InstanceModel
deflection = im
  (newDEModel' deflectionDM)
  (map qwUC [w_B, l_B, e_B, i_B]) -- FIXME: Other than the obvious type confliction, why should I need to convert the unitals to quantitydicts
  (qw y_B) -- FIXME: Same as above conflict
  []
  [dRef As.beamLoadingFunctionIntegrable]
  Nothing -- FIXME: Derivation
  "deflection"
  [S "Analyzing the deflection of the beam as a boundary value problem."]
  where
    deflectionRC :: RelationConcept
    deflectionRC = makeRC
      "deflectionRC"
      (nounPhraseSP "deflection as a relation concept")
      (S "deflection as a relation concept description") -- FIXME: description
      deflectionME

    deflectionME
      :: ModelExpr -- FIXME: This should technically have a universal quantifier for x
    deflectionME = (sy e_B `mulRe` sy i_B `mulRe` nthderiv 4 (sy y_B) x)
      $= apply1 w_B x
