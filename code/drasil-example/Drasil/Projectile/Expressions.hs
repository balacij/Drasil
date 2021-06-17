-- Assemble all of the mathematical expressions here, to promote re-use
--
-- This is supposed to always be imported qualified, since we're purposefully
-- overloading the names.
module Drasil.Projectile.Expressions where

import Prelude hiding (cos, sin)

import Language.Drasil
import Utils.Drasil
import qualified Data.Drasil.Quantities.Physics as QP (iSpeed, speed,
  constAccel, xConstAccel, yConstAccel, ixPos, iyPos, scalarPos)
import Data.Drasil.Quantities.Physics (gravitationalAccelConst, gravitationalAccel, 
  ixVel, iyVel, xPos, yPos, time, speed, iPos, scalarPos, xVel, yVel, xAccel, yAccel, 
  position, velocity, acceleration, constAccelV)

import Drasil.Projectile.Unitals (launAngle, launSpeed, targPos, tol, landPos, flightDur, offset)

flightDur', iyPos, yConstAccel, iSpeed :: Expr
flightDur' = 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst
iyPos = 0                                   --  launchOrigin
yConstAccel = - sy gravitationalAccelConst  -- accelYGravity
iSpeed = sy launSpeed

timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4 :: Expr
timeDerivEqn1 = sy yPos $= sy iyVel * sy time - sy gravitationalAccelConst * square (sy time) / 2
timeDerivEqn2 = sy iyVel * sy flightDur - sy gravitationalAccelConst * square (sy flightDur) / 2 $= 0
timeDerivEqn3 = sy iyVel - sy gravitationalAccelConst * sy flightDur / 2 $= 0
timeDerivEqn4 = sy flightDur $= 2 * sy iyVel / sy gravitationalAccelConst

landPosExpr, landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3 :: Expr
landPosExpr = 2 * square (sy launSpeed) * sin (sy launAngle) * cos (sy launAngle) / sy gravitationalAccelConst
landPosDerivEqn1 = sy xPos    $= sy ixVel * sy time
landPosDerivEqn2 = sy landPos $= sy ixVel * 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst
landPosDerivEqn3 = sy landPos $= sy launSpeed * cos (sy launAngle) * 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst

offset' :: Expr
offset' = sy landPos - sy targPos

message :: Expr
message = completeCase [case1, case2, case3]
  where case1 = (Str "The target was hit.",        abs (sy offset / sy targPos) $< sy tol)
        case2 = (Str "The projectile fell short.", sy offset $< 0)
        case3 = (Str "The projectile went long.",  sy offset $> 0)

--
speed' :: Expr
speed' = sy QP.iSpeed + sy QP.constAccel * sy time

rectVelDerivEqn1, rectVelDerivEqn2 :: Expr
rectVelDerivEqn1 = sy QP.constAccel $= deriv (sy speed) time
rectVelDerivEqn2 = defint (eqSymb speed) (sy QP.iSpeed) (sy speed) 1 $=
                   defint (eqSymb time) 0 (sy time) (sy QP.constAccel)

scalarPos' :: Expr
scalarPos' = sy iPos + sy QP.iSpeed * sy time + sy QP.constAccel * square (sy time) / 2

rectNoTime :: Expr
rectNoTime = square (sy QP.iSpeed) $= square (sy QP.iSpeed) + 2 * sy QP.constAccel * (sy scalarPos - sy iPos)

rectPosDerivEqn1, rectPosDerivEqn2, rectPosDerivEqn3 :: Expr
rectPosDerivEqn1 = sy speed $= deriv (sy scalarPos) time
rectPosDerivEqn2 = defint (eqSymb scalarPos) (sy iPos) (sy scalarPos) 1 $=
                   defint (eqSymb time) 0 (sy time) (sy speed)
rectPosDerivEqn3 = defint (eqSymb scalarPos) (sy iPos) (sy scalarPos) 1 $=
                   defint (eqSymb time) 0 (sy time) (sy QP.iSpeed + sy QP.constAccel * sy time)

--
velVecExpr :: Expr
velVecExpr = vec2D (sy ixVel + sy QP.xConstAccel * sy time) (sy iyVel + sy QP.yConstAccel * sy time)

--
posVecExpr :: Expr 
posVecExpr = vec2D
              (sy QP.ixPos + sy ixVel * sy time + sy QP.xConstAccel * square (sy time) / 2)
              (sy QP.iyPos + sy iyVel * sy time + sy QP.yConstAccel * square (sy time) / 2)

-- Helper expressions that represent the vectors of quantities as components
positionXY, velocityXY, accelerationXY, constAccelXY :: Expr
positionXY     = sy position     $= vec2D (sy xPos)           (sy yPos)
velocityXY     = sy velocity     $= vec2D (sy xVel)           (sy yVel)
accelerationXY = sy acceleration $= vec2D (sy xAccel)         (sy yAccel)
constAccelXY   = sy constAccelV  $= vec2D (sy QP.xConstAccel) (sy QP.yConstAccel)

-- Expressions for lesson
lcrectVel, lcrectPos, lcrectNoTime :: LabelledContent
lcrectVel = eqUnR (sy QP.speed $= speed') (makeEqnRef "rectVel")
lcrectPos = eqUnR (sy QP.scalarPos $= scalarPos') (makeEqnRef "rectPos")
lcrectNoTime = eqUnR (rectNoTime) (makeEqnRef "reactNoTime")

horMotionEqn1, horMotionEqn2 :: Expr
horMotionEqn1 = sy xVel $= sy ixVel
horMotionEqn2 = sy xPos $= sy QP.ixPos + sy ixVel * sy time

verMotionEqn1, verMotionEqn2, verMotionEqn3 :: Expr
verMotionEqn1 = sy yVel $= sy iyVel - sy gravitationalAccel * sy time
verMotionEqn2 = sy yPos $= sy QP.iyPos + sy iyVel * sy time - sy gravitationalAccel * square (sy time) / 2
verMotionEqn3 = square (sy yVel) $=  square ((sy iyVel)) - 2 * sy gravitationalAccel * (sy yPos - sy QP.iyPos) 
