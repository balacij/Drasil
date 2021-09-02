module Drasil.DblPendulum.ODEs (dblODEOptions, dblODEInfo) where

import Language.Drasil.Code (odeInfo, odeOptions, quantvar, ODEInfo, ODEMethod(RK45), ODEOptions)
import Language.Drasil.CodeExpr
import Data.Drasil.Quantities.Physics (time)
import Drasil.SWHS.Unitals (absTol, relTol, timeStep, timeFinal)
import Drasil.DblPendulum.Unitals
import Prelude hiding (sin, cos, sqrt)


dblODEOptions :: ODEOptions
dblODEOptions = odeOptions RK45 (sy absTol) (sy relTol) (sy timeStep) (exactDbl 0)
  
-- This is a second order ODE. The equation should be in the form of
-- variable substitution, i.e. u = y'. However here the the equation
-- can be defined in terms of the dependent variable itself because of the 
-- way scipy expects the function in python. 
dblODEInfo :: ODEInfo
dblODEInfo = odeInfo (quantvar time) -- Independent variable
             (quantvar sysOdeVariablesCon) -- Dependent variable
             [quantvar lenRodCon_1, quantvar lenRodCon_2, quantvar massCon_1, quantvar massCon_2] -- Other variables in the ODE
             (exactDbl 0) -- initial time
             (sy timeFinal) -- final time
             (exactDbl 0) -- Initial value of an ODE
             -- index 0 = theta1'
             -- index 1 = theta2'
             -- index 2 = theta1
             -- index 3 = theta2
             [idx (sy sysOdeVariablesCon) (int 0), -- theta1'
              idx (sy sysOdeVariablesCon) (int 1), -- theta2'
              neg(dbl 9.81) `mulRe`
                   (exactDbl 2 `mulRe` sy massCon_1 `addRe` sy massCon_2) `mulRe` sin (idx (sy sysOdeVariablesCon) (int 2) ) $-
                   (sy massCon_2 `mulRe` dbl 9.81 `mulRe`
                   sin (idx (sy sysOdeVariablesCon) (int 2) $- (exactDbl 2 `mulRe` idx (sy sysOdeVariablesCon) (int 3)))) $-
                   ((exactDbl 2 `mulRe` sin (idx (sy sysOdeVariablesCon) (int 2) $- idx (sy sysOdeVariablesCon) (int 3) )) `mulRe` sy massCon_2 `mulRe`
                   (
                        idx (sy sysOdeVariablesCon) (int 1)`mulRe` idx (sy sysOdeVariablesCon) (int 1) `mulRe` sy lenRodCon_2 `addRe` 
                        ((idx (sy sysOdeVariablesCon) (int 0) `mulRe` idx (sy sysOdeVariablesCon) (int 0)) `mulRe` sy lenRodCon_1 `mulRe` 
                        cos (idx (sy sysOdeVariablesCon) (int 2) $- idx (sy sysOdeVariablesCon) (int 3)))
                   ))
                   $/
                   sy lenRodCon_1 `mulRe` 
                   (
                       exactDbl 2 `mulRe` sy massCon_1 `addRe` sy massCon_2 $- 
                       (sy massCon_2 `mulRe` 
                       cos (exactDbl 2 `mulRe` idx (sy sysOdeVariablesCon) (int 2)  $- (exactDbl 2 `mulRe` idx (sy sysOdeVariablesCon) (int 3))))
                   ), -- theta1''
              exactDbl 2 `mulRe` sin (idx (sy sysOdeVariablesCon) (int 2) $- idx (sy sysOdeVariablesCon) (int 3)) `mulRe`
                   (
                        (idx (sy sysOdeVariablesCon) (int 1) `mulRe` idx (sy sysOdeVariablesCon) (int 1)) `mulRe` sy lenRodCon_1 `mulRe` (sy massCon_1 `addRe` sy massCon_2 ) `addRe`
                        (dbl 9.81 `mulRe` (sy massCon_1 `addRe` sy massCon_2 ) `mulRe` cos (idx (sy sysOdeVariablesCon) (int 2))) `addRe`
                        ((idx (sy sysOdeVariablesCon) (int 0) `mulRe` idx (sy sysOdeVariablesCon) (int 0)) `mulRe` sy lenRodCon_2 `mulRe` sy massCon_2 `mulRe` 
                        cos (idx (sy sysOdeVariablesCon) (int 2) $- idx (sy sysOdeVariablesCon) (int 3) ))
                   )
                   $/
                   sy lenRodCon_2 `mulRe` 
                   (
                        exactDbl 2 `mulRe` sy massCon_1 `addRe` sy massCon_2 $- 
                        (sy massCon_2 `mulRe` 
                        cos (exactDbl 2 `mulRe` idx (sy sysOdeVariablesCon) (int 2)  $- (exactDbl 2 `mulRe` idx (sy sysOdeVariablesCon) (int 3))))
                   )] -- theta2''
              -- ODE equations
             dblODEOptions
             -- Various options related to the ODE, 
             -- including solution method, step size, initial value of a second order ODE, etc.
