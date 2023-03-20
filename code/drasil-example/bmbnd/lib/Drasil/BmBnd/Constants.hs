module Drasil.BmBnd.Constants where

import           Language.Drasil
import           Drasil.BmBnd.Quantities (slender)

constants :: [ConstQDef]
constants = [slenderConst]

slenderConst :: ConstQDef
slenderConst = mkQuantDef slender (perc 1 1)
