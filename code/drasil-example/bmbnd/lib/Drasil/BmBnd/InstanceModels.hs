module Drasil.BmBnd.InstanceModels where

import           Theory.Drasil
import           Drasil.BmBnd.Quantities (y_B, w_B, l_B, e_B, i_B)
import           Language.Drasil
import qualified Drasil.BmBnd.Assumptions as As

models :: [InstanceModel]
models = [deflection]

deflection :: InstanceModel
deflection = im
  (othModel
     "deflectionIM"
     (nounPhraseSP "deflection as an instance model")
     deflectionRC)
  (map qwUC [w_B, l_B, e_B, i_B]) -- FIXME: Other than the obvious type confliction, why should I need to convert the unitals to quantitydicts
  (qw y_B) -- FIXME: Same as above conflict
  []
  [dRef As.world] -- FIXME: I need to have a better 'decorated reference' here
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

    deflectionME :: ModelExpr
    deflectionME = sy e_B
