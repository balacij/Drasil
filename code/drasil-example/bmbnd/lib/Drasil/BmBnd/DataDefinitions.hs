module Drasil.BmBnd.DataDefinitions where

import           Theory.Drasil
import           Language.Drasil
import           Drasil.BmBnd.Quantities
import qualified Drasil.BmBnd.Assumptions as As

models :: [DataDefinition]
models = [load]

load :: DataDefinition
load = ddE loadingFunction [dRef As.world] Nothing "loading" []

loadingFunction :: QDefinition Expr
loadingFunction = mkFuncDefByQ w_B [x] (sy x)
