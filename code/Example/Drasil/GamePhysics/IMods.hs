module Drasil.GamePhysics.IMods where

--import qualified Drasil.GamePhysics.Unitals as GPUN

import Language.Drasil
import Data.Drasil.Utils (foldle1)
import Data.Drasil.SentenceStructures (foldlSent)
import qualified Data.Drasil.Quantities.Math as QM (orientation)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import qualified Data.Drasil.Quantities.Physics as QP (torque, acceleration, 
  angularAccel, force, gravitationalAccel, velocity, 
  momentOfInertia, angularVelocity, position)
--import qualified Data.Drasil.Concepts.Math as QM ()
--import Data.Drasil.Quantities.Physics
import Prelude hiding (id)
import Control.Lens ((^.))


iModels :: [RelationConcept]
iModels = [im1, im2, im3]


{-- Force on the translational motion  --}
im1 :: RelationConcept
im1 = makeRC "im1" (im1NP) (im1descr +:+ im1leg) im1Rel 

im1NP :: NP
im1NP =  nounPhraseSP "Force on the translational motion of a set of 2d rigid bodies"

im1Rel :: Relation -- FIXME: add proper equation
im1Rel = (C QP.acceleration) := (C QP.velocity) := (C QP.gravitationalAccel) + ((C QP.force) / (C QPP.mass))

im1descr, im1leg :: Sentence
im1descr = foldlSent [S "The above equation expresses the total", 
  (phrase $ QP.acceleration ^. term), S "of the", (phrase $ CP.rigidBody ^. term), 
  S "(A1, A2) i as the sum of", (phrase $ QP.gravitationalAccel ^. term), 
  S "(GD3) and", (phrase $ QP.acceleration ^. term), S "due to applied", 
  (phrase $ QP.force ^. term), S "Fi(t) (T1). The resultant outputs are", 
  S "then obtained from this equation using DD2, DD3 and DD4. It is currently", 
  S "assumed that there is no damping (A6) or constraints (A7) involved"]

im1leg = foldle1 (+:+.) (+:+.) 
  [S "mi is the mass of the i-th rigid body (kg)",
  S "g is the acceleration due to gravity (ms-2)",
  S "t is a point in time and t0 denotes the initial time (s)",
  (helper1 QP.position "i" 
  (S "specifically the position of its center of mass, pCM(t) (DD1))")),
  (helper1 QP.acceleration "i" EmptyS),
  (helper1 QP.velocity "i" EmptyS),
  S "F(t) is the force applied to the i-th body at time t (N)"]

{-- --}

im2 :: RelationConcept
im2 = makeRC "im2" (im2NP) (im2descr +:+ im2leg) im2Rel 

im2NP :: NP
im2NP =  nounPhraseSP "Force on the rotational motion of a set of 2D rigid body"

im2Rel :: Relation -- FIXME: add proper equation
im2Rel = (C QP.angularAccel) := (C QP.angularVelocity) := ((C QP.torque) / (C QP.momentOfInertia))

im2descr, im2leg :: Sentence
im2descr = foldlSent [S "The above equation for the total angular acceleration", 
  S "of the rigid body (A1, A2) i is derived from T5, and the resultant outputs",
  S "are then obtained from this equation using DD5, DD6 and DD7. It is",
  S "currently assumed that there is no damping (A6) or constraints (A7) involved"]

im2leg = foldle1 (+:+.) (+:+.) 
  [S "mi is the mass of the i-th rigid body (kg)",
  S "g is the acceleration due to gravity (ms-2)",
  S "t is a point in time and t0 denotes the initial time (s)",
  (helper1 QM.orientation "i" EmptyS),
  (helper1 QP.angularVelocity "i" EmptyS),
  (helper1 QP.angularAccel "k" EmptyS),
  S "t i(t) is the torque applied to the i-th body at time t (N m)",
  S "Signed direction of torque is defined by (A4)",
  S "Ii is the moment of inertia of the i-th body (kg m2)"]

{-- --}

im3 :: RelationConcept
im3 = makeRC "im3" (im3NP) (im3descr +:+ im3leg) im3Rel 

im3NP :: NP
im3NP =  nounPhraseSP "Collisions on 2D rigid bodies"

im3Rel :: Relation -- FIXME: add proper equation
im3Rel = (C QP.force) := (C QPP.mass)

im3descr, im3leg :: Sentence
im3descr = foldlSent [S "This instance model is based on our assumptions",
  S "regarding rigid body (A1, A2) collisions (A5). Again, this does not take",
  S "damping (A6) or constraints (A7) into account"]

im3leg = foldle1 (+:+.) (+:+.) 
  [S "mk is the mass of the k-th rigid body (kg)",
  S "Ik is the moment of inertia of the k-th rigid body (kg m2)",
  S "t is a point in time, t0 denotes the initial time" `sC` 
  S "and tc denotes the time at collision (s)",
  (helper1 QP.position "i" (S "specifically the position of its center of mass, pCM(t) (DD1))")),
  (helper1 QP.velocity "k" EmptyS),
  (helper1 QM.orientation "k" EmptyS),
  (helper1 QP.angularVelocity "k" EmptyS), 
  S "n is the collision normal vector (m)", 
  S "Its signed direction is determined by (A4)",
  S "j is the collision impulse (DD8) (N s)", 
  S "P is the point of collision (m)",
  S "rkP is the displacement vector between the center of mass" +:+
  S "of the k-th body and point P (m)"]

{-- __n(t) is the n-th body's __ -option- at time t (units) --}
helper1 :: (Unitary c, SymbolForm c) => c -> String -> Sentence -> Sentence
helper1 t i EmptyS = (P $ t ^. symbol) :+: (S i) :+: (S "(t)") +:+ 
  S "is the" +:+ (S i) :+: (S "-th body's") +:+ (phrase $ t ^. term) +:+
  S "at time t" +:+ (sParen $ Sy $ unit_symb t)
helper1 t i opt = (P $ t ^. symbol) :+: (S i) :+: (S "(t)") +:+ 
  S "is the" +:+ (S i) :+: (S "-th body's") +:+ (phrase $ t ^. term) +:+ opt +:+
  S "at time t" +:+ (sParen $ Sy $ unit_symb t)