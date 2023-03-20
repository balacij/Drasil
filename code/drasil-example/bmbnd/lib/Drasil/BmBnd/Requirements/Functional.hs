module Drasil.BmBnd.Requirements.Functional where

import           Language.Drasil
import           Data.Drasil.Concepts.Documentation (funcReqDom)

requirements :: [ConceptInstance]
requirements = [inputDisplay, inputValidation, calculate, output]

inputDisplay :: ConceptInstance
inputDisplay = cic "inputDisplay" (S "") "inputDisplay" funcReqDom

inputValidation :: ConceptInstance
inputValidation = cic "inputValidation" (S "") "inputValidation" funcReqDom

calculate :: ConceptInstance
calculate = cic "calculate" (S "") "calculate" funcReqDom

output :: ConceptInstance
output = cic "output" (S "") "output" funcReqDom
