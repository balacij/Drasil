module Drasil.BmBnd.Requirements.NonFunctional where

import           Language.Drasil
import           Data.Drasil.Concepts.Documentation

requirements :: [ConceptInstance]
requirements =
  [accuracy, usability, maintainability, portability, verifiable, reusable]

accuracy :: ConceptInstance
accuracy = cic "accuracy" (S "") "accuracy" nonFuncReqDom

usability :: ConceptInstance
usability = cic "usability" (S "") "usability" nonFuncReqDom

maintainability :: ConceptInstance
maintainability = cic "maintainability" (S "") "maintainability" nonFuncReqDom

portability :: ConceptInstance
portability = cic "portability" (S "") "portability" nonFuncReqDom

verifiable :: ConceptInstance
verifiable = cic "verifiable" (S "") "verifiable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (S "") "reusable" nonFuncReqDom
