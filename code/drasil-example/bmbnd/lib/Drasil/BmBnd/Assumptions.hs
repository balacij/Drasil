module Drasil.BmBnd.Assumptions where

import           Language.Drasil
import           Data.Drasil.Concepts.Documentation

assumptions :: [ConceptInstance]
assumptions =
  [ worldDimension
  , worldTime
  , beamSlender
  , beamPrismatic
  , beamUniformCrossSection
  , beamFlat
  , beamConstantSecondMomentOfArea
  , beamVerticalLinearElasticLoad
  , beamConstantModulusOfElasticity
  , beamSmallDeflections
  , beamLocallySmallSlopes
  , beamSimplySupported
  , beamLoadingPolynomial
  , beamNoPointLoads
  , beamNoAxialLoading
  , beamDeflectionFunctionDifferentiable
  , beamLoadingFunctionIntegrable]

worldDimension :: ConceptInstance
worldDimension = cic
  "worldDimension" -- human-readable label (i.e., id/href _text_)
  (S "The world model is two-dimensional.")
  "worldDimension" -- internal label (i.e., id/href)
  assumpDom

worldTime :: ConceptInstance
worldTime = cic
  "worldTime" -- human-readable label (i.e., id/href _text_)
  (S "The world model observes the instant a load is applied on the beam.")
  "worldTime" -- internal label (i.e., id/href)
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
  (S "The beam is straight/flat within a reasonable tolerance.")
  "beamFlat"
  assumpDom

beamConstantSecondMomentOfArea :: ConceptInstance
beamConstantSecondMomentOfArea = cic
  "beamConstantSecondMomentOfArea"
  (S "The beam has a constant second moment of area.")
  "beamConstantSecondMomentOfArea"
  assumpDom

beamVerticalLinearElasticLoad :: ConceptInstance
beamVerticalLinearElasticLoad = cic
  "beamVerticalLinearElasticLoad"
  (S "The beam experiences vertical linear elastic load.")
  "beamVerticalLinearElasticLoad"
  assumpDom

beamConstantModulusOfElasticity :: ConceptInstance
beamConstantModulusOfElasticity = cic
  "beamConstantModulusOfElasticity"
  (S "The beam's modulus of elasticity is constant along the beam.")
  "beamConstantModulusOfElasticity"
  assumpDom

beamSmallDeflections :: ConceptInstance
beamSmallDeflections = cic
  "beamSmallDeflections"
  (S
     "Only relatively small deflections will be examined (whereby the maximum deflection is at most SLENDER of the beam's length).")
  "beamSmallDeflections"
  assumpDom

beamLocallySmallSlopes :: ConceptInstance
beamLocallySmallSlopes = cic
  "beamLocallySmallSlopes"
  (S
     "The deflection of the beam will have locally small slopes across the beam.")
  "beamLocallySmallSlopes"
  assumpDom

beamSimplySupported :: ConceptInstance
beamSimplySupported = cic
  "beamSimplySupported"
  (S "The beam is simply supported.")
  "beamSimplySupported"
  assumpDom

beamLoadingPolynomial :: ConceptInstance
beamLoadingPolynomial = cic
  "beamLoadingPolynomial"
  (S
     "The beam's loading may be captured by a third-order polynomial of standard form.")
  "beamLoadingPolynomial"
  assumpDom

beamNoPointLoads :: ConceptInstance
beamNoPointLoads = cic
  "beamNoPointLoads"
  (S "The beam's loading contains no point loads.")
  "beamNoPointLoads"
  assumpDom

beamNoAxialLoading :: ConceptInstance
beamNoAxialLoading = cic
  "beamNoAxialLoading"
  (S "The beam has no loading applied axially.")
  "beamNoAxialLoading"
  assumpDom

beamDeflectionFunctionDifferentiable :: ConceptInstance
beamDeflectionFunctionDifferentiable = cic
  "beamDeflectionFunctionDifferentiable"
  (S
     "The beam deflection function is continuously differentiable 4 times on [0, L].") -- FIXME: While this is textually 'good', I would prefer it be written using the math notation "y_B(x) \in C^4"
  "beamDeflectionFunctionDifferentiable"
  assumpDom

beamLoadingFunctionIntegrable :: ConceptInstance
beamLoadingFunctionIntegrable = cic
  "beamLoadingFunctionIntegrable"
  (S "The beam loading function is integrable 4 times on [0, L].") -- TODO: Is this really needed? I feel like it should be covered by beamDeflectionFunctionDifferentiable
  "beamLoadingFunctionIntegrable"
  assumpDom
