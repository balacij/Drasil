module BVPCheatModule where

import           Language.Drasil
import           Language.Drasil.Code
import           Drasil.BmBnd.Quantities (cheat, x)
import           Language.Drasil.Printers

bvpCheatModule :: Mod
bvpCheatModule = packmodRequires
  "BVPCheats"
  "Hand-written BVP solver for the beam bending analysis program."
  ["scipy"]
  []
  [bvpCheat]

bvpCheat :: Func
bvpCheat = funcDef
  (showHasSymbImpl cheat)
  "Linearly interpolates a z value at given x and y values"
  [x]
  (Vect Real)
  (Just "z value interpolated at given x and y values")
  [FRet $ sy x]
