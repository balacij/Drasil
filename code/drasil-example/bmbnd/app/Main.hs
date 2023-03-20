module Main (main) where

import           Drasil.SRSDocument
import           Language.Drasil
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           Drasil.BmBnd.Meta (srs, fullSI)
import           Language.Drasil.Generate (gen, typeCheckSI, DocSpec(DocSpec)
                                         , DocType(SRS), Format(..), docChoices
                                         , genLog, genDot)

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

main :: IO ()
main = do
  setLocaleEncoding utf8
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX]) "BmBnd_SRS") srs printSetting
  genDot fullSI
  genLog fullSI printSetting
