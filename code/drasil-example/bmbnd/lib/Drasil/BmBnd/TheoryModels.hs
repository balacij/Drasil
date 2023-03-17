module Drasil.BmBnd.TheoryModels where

import           Theory.Drasil
import           Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Data.List.NonEmpty as NE
import           Drasil.BmBnd.Quantities
import qualified Drasil.BmBnd.Assumptions as As

models :: [TheoryModel]
models = [curvature]

curvature :: TheoryModel
curvature = tm
  (equationalConstraints' curvatureCS)
  ([] :: [QuantityDict]) -- FIXME: I should not need to manually define the type signature for this to type-check.
  ([] :: [ConceptChunk])
  []
  [curveRel] -- FIXME: I should not need to manually reference curveRel twice.
  []
  [dRef As.world] -- FIXME: This is the wrong 'source', but I really shouldn't _need_ a source for my work.
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
