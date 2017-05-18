module Data.Drasil.Changes where

import Language.Drasil
import Data.Drasil.Modules

{-likely changes-}

lcHW :: LCChunk
lcHW      = LCChunk (nw $ npnc "hardware" (nounPhraseSP
  "The specific hardware on which the software is running.")
  ) [mod_hw]

lcCtrl, lcInputF, lcOutputF, lcRng, lcPlot :: ModuleChunk -> LCChunk

lcCtrl    mod_ctrl    = LCChunk (nw $ npnc "control" (nounPhraseSP
  "The algorithm for the overall operation procedure of the program.")
  ) [mod_ctrl]
  
lcInputF  mod_inputf  = LCChunk (nw $ npnc "input" (nounPhraseSP
  "The format of the initial input data.")
  ) [mod_inputf]
  
lcOutputF mod_outputf = LCChunk (nw $ npnc "output" (nounPhraseSP
  "The format of the final output data.")
  ) [mod_outputf]

lcRng     mod_rng     = LCChunk (nw $ npnc "rand" (nounPhraseSP
  "The method of generating pseudo-random numbers.")
  ) [mod_rng]
  
lcPlot    mod_plot    = LCChunk (nw $ npnc "plot" (nounPhraseSP
  "The method of displaying the final output.")
  ) [mod_plot]


{-Unlikely Changes-}

ucIO, ucInputS, ucOutput :: UCChunk

ucIO     = nw $ npnc "IO" (nounPhraseSP $
  "Input/Output devices (Input: File and/or Keyboard, Output: File, " ++
  "Memory, and/or Screen).")

ucInputS = nw $ npnc "inputsource" (nounPhraseSP
  "There will always be a source of input data external to the software.")

ucOutput = nw $ npnc "output" (nounPhraseSP
  "Output data are displayed to the output device.")