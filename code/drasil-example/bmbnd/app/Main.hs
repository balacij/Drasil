module Main (main) where

import           Drasil.SRSDocument
import           Language.Drasil
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           Drasil.BmBnd.Meta (srs, fullSI)
import           Language.Drasil.Generate
import           Language.Drasil.Code

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

main :: IO ()
main = do
  setLocaleEncoding utf8
  typeCheckSI fullSI
  gen (DocSpec (docChoices SRS [HTML, TeX]) "BmBnd_SRS") srs printSetting
  genCode codeChoices bCodeSpec
  genDot fullSI
  genLog fullSI printSetting

bCodeSpec :: CodeSpec
bCodeSpec = codeSpec fullSI codeChoices []

codeChoices :: Choices
codeChoices =
  defaultChoices { lang = [Cpp, Java, CSharp, Swift]
                 , architecture = makeArchit (Modular Combined) Program
                 , dataInfo = makeData Unbundled (Store Bundled) Const
                 , optFeats = makeOptFeats
                     (makeDocConfig
                        [CommentFunc, CommentClass, CommentMod]
                        Quiet
                        Hide)
                     (makeLogConfig [] "log.txt")
                     [ReadME]
                 , srsConstraints = makeConstraints Warning Warning
                 , extLibs = []
                 }
