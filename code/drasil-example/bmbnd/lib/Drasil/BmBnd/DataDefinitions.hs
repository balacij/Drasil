module Drasil.BmBnd.DataDefinitions where

import           Theory.Drasil
import           Language.Drasil
import           Drasil.BmBnd.Quantities
import qualified Drasil.BmBnd.Assumptions as As
import Drasil.BmBnd.InstanceModels (deflection)

models :: [DataDefinition]
models = [load, deflectionCheat]

load :: DataDefinition
load = ddE loadingFunction [dRef As.beamLoadingPolynomial] Nothing "loading" []

loadingFunction :: QDefinition Expr
loadingFunction = mkFuncDefByQ w_B [x]
  $ sy a_0
  `addRe` (sy a_1 `mulRe` sy x)
  `addRe` (sy a_2 `mulRe` (sy x $^ int 2))
  `addRe` (sy a_3 `mulRe` (sy x $^ int 3))

deflectionCheat :: DataDefinition
deflectionCheat = ddE y_BQD [dRef deflection] Nothing "" []

y_BQD :: QDefinition Expr
y_BQD = mkQuantDef y_B (apply cheat [])
