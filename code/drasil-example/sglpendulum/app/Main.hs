module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Generate (gen, typeCheckSI, genDot, genLog, DocSpec(DocSpec), DocType(SRS), Format(..), docChoices)
import Drasil.SglPendulum.Body (srs, printSetting, fullSI)


main :: IO()
main = do
  setLocaleEncoding utf8
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX, JSON]) "SglPendulum_SRS") srs printSetting
  genDot fullSI
  genLog fullSI printSetting
