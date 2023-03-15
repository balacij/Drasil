module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Drasil.BmBnd.Body (printSetting, srs, fullSI)
import Language.Drasil.Generate (gen, typeCheckSI, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)


main :: IO ()
main = do
  setLocaleEncoding utf8
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX]) "BmBnd_SRS") srs printSetting
