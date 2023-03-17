module Drasil.BmBnd.Figures where

import           Language.Drasil

resourcePath :: String
resourcePath = "../../../../datafiles/bmbnd/"

bmBndDiagramSimplified :: LabelledContent
bmBndDiagramSimplified = llcc (makeFigRef "bmbndSimplified")
  $ figWithWidth
    (S "simplified system")
    -- (resourcePath ++ "beam_bending_diagram.drawio.pdf") -- FIXME: I should be able to inline PDFs in addition to "images"! iframe instead of img tags
    (resourcePath ++ "beam_bending_diagram.drawio.png")
    70

bmBndDiagram :: LabelledContent
bmBndDiagram = llcc (makeFigRef "bmbndFull")
  $ figWithWidth
    (S "physical system")
    -- (resourcePath ++ "beam_bending_diagram_annotated.drawio.pdf") -- FIXME: I should be able to inline PDFs in addition to "images"! iframe instead of img tags
    (resourcePath ++ "beam_bending_diagram_annotated.drawio.png")
    70

systemContext :: LabelledContent
systemContext = llcc (makeFigRef "sysCxt")
  $ figWithWidth (S "system context") (resourcePath ++ "system context.png") 70
