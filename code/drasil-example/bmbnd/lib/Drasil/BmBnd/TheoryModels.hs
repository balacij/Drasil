module Drasil.BmBnd.TheoryModels where

import           Theory.Drasil
import           Language.Drasil
import qualified Data.List.NonEmpty as NE
import           Drasil.BmBnd.Quantities
import qualified Drasil.BmBnd.Assumptions as As

models :: [TheoryModel]
models =
  [curvature, prismaticBeamArc, elasticCurveODE, eulerBernoulliBeamDeflection]

curvature :: TheoryModel
curvature = tm
  (equationalConstraints' curvatureCS)
  ([] :: [QuantityDict]) -- FIXME: I should not need to manually define the type signature for this to type-check.
  [curveConceptChunk] -- FIXME: Why do I need this?
  []
  [curveRel] -- FIXME: I should not need to manually reference curveRel twice.
  []
  [dRef As.world] -- FIXME: This is the wrong 'source', but I really shouldn't _need_ a source for the code to compile.
  "curvatureThm"
  [] -- FIXME: NOTES!
  where
    curveConceptChunk =
      dccWDS "curve" (nounPhraseSP "Curvature of a plane") (S "") -- FIXME: ?

    curveRel :: ModelExpr
    curveRel = (exactDbl 1 $/ sy rho)
      $= (nthderiv 2 (sy f) a
          $/ ((exactDbl 1 `addRe` (deriv (sy f) a $^ exactDbl 2)) $^ frac 3 2))

    curvatureCS = mkConstraintSet curveConceptChunk $ NE.fromList [curveRel]

prismaticBeamArc :: TheoryModel
prismaticBeamArc = tm
  (equationalConstraints' arcCS)
  ([] :: [QuantityDict])
  [arcConceptChunk]
  []
  [arcRel]
  []
  [dRef As.world] -- FIXME: This is the wrong 'source', but I really shouldn't _need_ a source for the code to compile.
  "prismaticBeamArc"
  []
  where
    arcConceptChunk = dccWDS "arc" (nounPhraseSP "Curvature of a plane") (S "") -- FIXME: ?

    arcRel :: ModelExpr
    arcRel = (exactDbl 1 $/ sy rho) $= (apply1 moment x $/ (sy e `mulRe` sy i))

    arcCS = mkConstraintSet arcConceptChunk $ NE.fromList [arcRel]

elasticCurveODE :: TheoryModel
elasticCurveODE = tm
  (equationalConstraints' elasticCurveCS)
  ([] :: [QuantityDict])
  [elasticCurveConceptChunk]
  []
  [elasticCurveEqn]
  []
  [dRef As.world] -- FIXME: This is the wrong 'source', but I really shouldn't _need_ a source for the code to compile.
  "elasticCurveODE"
  []
  where
    elasticCurveConceptChunk = dccWDS
      "elasticCurve"
      (nounPhraseSP "Simply supported beam bending curve")
      (S "") -- FIXME: ?

    elasticCurveEqn :: ModelExpr
    elasticCurveEqn = nthderiv 2 (sy y) x
      $= (apply1 moment x $/ (sy e `mulRe` sy i))

    elasticCurveCS =
      mkConstraintSet elasticCurveConceptChunk $ NE.fromList [elasticCurveEqn] -- FIXME: Add the boundary conditions

eulerBernoulliBeamDeflection :: TheoryModel
eulerBernoulliBeamDeflection = tm
  (equationalConstraints' eulerBernoulliBeamDeflectionCS)
  ([] :: [QuantityDict])
  [eulerBernoulliBeamDeflectionConceptChunk]
  []
  [eulerBernoulliBeamDeflectionEqn]
  []
  [dRef As.world] -- FIXME: This is the wrong 'source', but I really shouldn't _need_ a source for the code to compile.
  "eulerBernoulliBeamDeflection"
  []
  where
    eulerBernoulliBeamDeflectionConceptChunk = dccWDS
      "eulerBernoulliBeamDeflection"
      (nounPhraseSP "Euler-Bernoulli Beam Deflection")
      (S "") -- FIXME: ?

    eulerBernoulliBeamDeflectionEqn :: ModelExpr
    eulerBernoulliBeamDeflectionEqn =
      (sy e `mulRe` sy i `mulRe` nthderiv 4 (sy y) x) $= apply1 w x

    eulerBernoulliBeamDeflectionCS = mkConstraintSet
      eulerBernoulliBeamDeflectionConceptChunk
      $ NE.fromList [eulerBernoulliBeamDeflectionEqn]
