module Drasil.BmBnd.Quantities where

import           Language.Drasil
import           Language.Drasil.ShortHands
import           Data.Drasil.SI_Units
import           Data.Drasil.Units.Physics

quantities :: [QuantityDict]
quantities = map qw unitals

inputs, outputs :: [QuantityDict]
inputs = map qw inputUnitals

outputs = map qw outputUnitals

unitals :: [UnitalChunk]
unitals =
  intermediateUnitals ++ abstractUnitals ++ inputUnitals ++ outputUnitals

intermediateUnitals :: [UnitalChunk]
intermediateUnitals = [x, w_B]

abstractUnitals :: [UnitalChunk]
abstractUnitals = [l, e, i, y]

inputUnitals :: [UnitalChunk]
inputUnitals = [a_0, a_1, a_2, a_3, l_B, e_B, i_B]

outputUnitals :: [UnitalChunk]
outputUnitals = [y_B]

-- QUESTION: If we only store QuantityDicts, why do we bother with UnitalChunks
--           as a type? (see: `qw x` under quantities list)
x :: UnitalChunk
x = uc'
  "x"
  (nounPhraseSent
   $ S
     "one-dimensional point along the beam, from the left-hand side (at the pinned support)")
  (S "point")
  lX
  Real
  metre

w_B :: UnitalChunk
w_B = uc'
  "w_b"
  (nounPhraseSent $ S "load at a particular point along the beam")
  (S "loading function")
  (sub lW cB)
  (mkFunction [Real] Real)
  newton

l_B :: UnitalChunk
l_B = uc'
  "l_b"
  (nounPhraseSent $ S "length of the beam")
  (S "length")
  (sub cL cB)
  Real
  metre

e_B :: UnitalChunk
e_B = uc'
  "e_b"
  (nounPhraseSent $ S "modulus of elasticity of the beam")
  (S "modulus of elasticity")
  (sub cE cB)
  Real
  pascal

i_B :: UnitalChunk
i_B = uc'
  "i_b"
  (nounPhraseSent $ S "moment of second area of a cross-section of the beam")
  (S "moment of second area")
  (sub cI cB)
  Real
  metre

y_B :: UnitalChunk
y_B = uc'
  "y_b"
  (nounPhraseSent $ S "deflection at a particular point along the beam")
  (S "deflection function")
  (sub lY cB)
  (mkFunction [Real] Real)
  metre

l :: UnitalChunk
l = uc'
  "l"
  (nounPhraseSent $ S "length of the abstract beam")
  (S "length")
  cL
  Real
  metre

e :: UnitalChunk
e = uc'
  "e"
  (nounPhraseSent $ S "modulus of elasticity of the abstract beam")
  (S "modulus of elasticity")
  cE
  Real
  pascal

i :: UnitalChunk
i = uc'
  "i"
  (nounPhraseSent
   $ S "moment of second area of a cross-section of the abstract beam")
  (S "moment of second area")
  cI
  Real
  metre

y :: UnitalChunk
y = uc'
  "y"
  (nounPhraseSent
   $ S "deflection at a particular point along the abstract beam")
  (S "deflection function")
  lY
  (mkFunction [Real] Real)
  metre

-- TODO: I have 2 things to implicitly keep in mind (which I shouldn't) while I write these unital chunks:
--       1) the strings should start with a lowercase letter, and
--       2) the strings should not end with a period.
-- FIXME: How can I switch from greek letters to a_0, a_1, etc.?
a_n :: Int -> UnitalChunk
a_n n
  | n >= 0 = uc'
    ("a_" ++ show n)
    (nounPhraseSent s)
    s
    (subStr lA $ show n)
    Real
    forcePerMeterU -- FIXME: Typo in stdlib? 'metre'?
  | otherwise = error "'a_n' only allows non-negative 'n's"
  where
    s = S $ "coefficient of w_B's term of power " ++ show n -- FIXME: I'm really cheating with this formulation of text. Try re-writing it in another way! :)

a_0, a_1, a_2, a_3 :: UnitalChunk
a_0 = a_n 0

a_1 = a_n 1

a_2 = a_n 2

a_3 = a_n 3
