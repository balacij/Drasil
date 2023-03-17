module Drasil.BmBnd.TheoryModels where

import           Theory.Drasil
import           Language.Drasil
import qualified Data.List.NonEmpty as NE
import           Drasil.BmBnd.Quantities
import qualified Drasil.BmBnd.Assumptions as As

models :: [TheoryModel]
models = [curvature, prismaticBeamArc]

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
    arcConceptChunk =
      dccWDS "arc" (nounPhraseSP "Curvature of a plane") (S "") -- FIXME: ?

    arcRel :: ModelExpr
    arcRel = (exactDbl 1 $/ sy rho)
      $= (apply1 moment x
          $/ (sy e `mulRe` sy i))

    arcCS = mkConstraintSet arcConceptChunk $ NE.fromList [arcRel]
