module Drasil.BmBnd.Assumptions where

import           Language.Drasil
import           Data.Drasil.Concepts.Documentation

assumptions :: [ConceptInstance]
assumptions = [ world
              , beamSlender
              , beamPrismatic
              , beamUniformCrossSection
              , beamFlat
              , beamStaticSecondMomentOfArea
              , beamVerticalLinearElasticLoad
              , beamStaticModulusOfElasticity]

world :: ConceptInstance
world = cic
  "world" -- human-readable label (i.e., id/href _text_)
  (S
     "The world model is two-dimensional, observing the instant a load is applied on the beam.")
  "world" -- internal label (i.e., id/href)
  assumpDom

beamSlender :: ConceptInstance
beamSlender =
  cic "beamSlender" (S "The beam is slender.") "beamSlender" assumpDom

beamPrismatic :: ConceptInstance
beamPrismatic =
  cic "beamPrismatic" (S "The beam is prismatic.") "beamPrismatic" assumpDom

beamUniformCrossSection :: ConceptInstance
beamUniformCrossSection = cic
  "beamUniformCrossSection"
  (S "The beam has a uniform cross-section.")
  "beamUniformCrossSection"
  assumpDom

beamFlat :: ConceptInstance
beamFlat = cic
  "beamFlat"
  (S "The beam is flat within a reasonable tolerance.")
  "beamFlat"
  assumpDom

beamStaticSecondMomentOfArea :: ConceptInstance
beamStaticSecondMomentOfArea = cic
  "beamStaticSecondMomentOfArea"
  (S "The beam has a static second moment of area.")
  "beamStaticSecondMomentOfArea"
  assumpDom

beamVerticalLinearElasticLoad :: ConceptInstance
beamVerticalLinearElasticLoad = cic
  "beamVerticalLinearElasticLoad"
  (S "The beam experiences vertical linear elastic load.")
  "beamVerticalLinearElasticLoad"
  assumpDom

beamStaticModulusOfElasticity :: ConceptInstance
beamStaticModulusOfElasticity = cic
  "beamStaticModulusOfElasticity"
  (S "The beam's modulus of elasticity is static along the beam.")
  "beamStaticModulusOfElasticity"
  assumpDom
