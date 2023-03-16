module Drasil.BmBnd.Quantities where

import Language.Drasil
import Language.Drasil.ShortHands
import Data.Drasil.SI_Units
import Data.Drasil.Units.Physics

quantities :: [QuantityDict]
quantities = [qw x, qw w_B]

inputs, outputs :: [QuantityDict]
inputs = []
outputs = []

-- QUESTION: If we only store QuantityDicts, why do we bother with UnitalChunks as a type?
--           (see: `qw x` under quantities list)
x :: UnitalChunk
x = uc'
  "x"
  (nounPhraseSent $ S "Point")
  (S "A one-dimensional point along the beam, from the left-hand side.")
  lX
  Real
  metre

w_B :: UnitalChunk
w_B = uc'
    "w_b"
    (nounPhraseSent $ S "Loading function")
    (S "")
    (sub lW cB)
    (mkFunction [Real] Real)
    forcePerMeterU
