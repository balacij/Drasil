module Drasil.GlassBR.Body where
import Control.Lens ((^.))

import Language.Drasil 
import Data.Drasil.SI_Units
import Data.Drasil.Authors
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (matrix, traceyMatrix, graph, calculation)
import Prelude hiding (id)

import           Drasil.TableOfUnits
import           Drasil.TableOfSymbols
import           Drasil.TableOfAbbAndAcronyms
import           Drasil.OrganizationOfSRS
import qualified Drasil.SRS as SRS
import           Drasil.ReferenceMaterial

import Drasil.GlassBR.Example
import Drasil.GlassBR.Concepts
import Drasil.GlassBR.Changes
import Drasil.GlassBR.Modules
import Drasil.GlassBR.Reqs

import Drasil.DocumentLanguage

this_si :: [UnitDefn]
this_si = map UU [metre, second] ++ map UU [pascal, newton]

s2, s2_1, s2_2, s2_3, s3, s3_1, s3_2, s4, s4_1, s4_2,
  s5, s5_1, s5_2, s6, s6_1, s6_1_1, s6_1_2, s6_1_3, s6_2, s6_2_1, s6_2_2, 
  s6_2_3, s6_2_4, s6_2_5, s7, s7_1, s7_2, s8, s9, s10, s11 :: Section

s2_intro, s2_2_intro, s3_intro, 
  s3_1_intro, s3_2_intro, s4_intro, s4_1_bullets, s4_2_intro, s5_intro, 
  s5_1_table, s5_2_bullets, s6_intro, s6_1_intro, s6_1_1_intro, s6_1_1_bullets,
  s6_1_2_intro, s6_1_2_list, s6_1_3_list, s6_2_intro, s6_2_1_intro, 
  s6_2_4_intro, s6_2_5_intro, --s6_2_5_table1, 
  s6_2_5_table2, s6_2_5_intro2, --s6_2_5_table3, 
  s7_1_intro, s7_2_intro, s8_list, s9_intro1, s9_table1, s9_table2, s9_table3,
  s10_list, s11_intro, fig_glassbr, fig_2, fig_3, fig_4, 
  fig_5, fig_6 :: Contents

s2_1_intro, s6_2_1_list, s7_1_list, s9_intro2 :: [Contents]

srs_authors, mg_authors, s2_3_intro_end, s2_3_intro :: Sentence
srs_authors = manyNames [nikitha,spencerSmith]
mg_authors = manyNames [spencerSmith,thulasi]

authors :: People
authors = [nikitha, spencerSmith]

glassBR_srs' :: Document
glassBR_srs' = mkDoc' mkSRS for glassSystInfo

--FIXME: Missing ToS intro because this example was using the default 
-- (nuclear literature related) intro.

--FIXME: Should "blast risk" be its own chunk?
mkSRS :: DocDesc 
mkSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA]) :
  map Verbatim [s2,s3,s4,s5,s6,s7,s8,s9,s10,s11]
  
glassSystInfo :: SystemInformation
glassSystInfo = SI glassBRProg srs authors this_si this_symbols ([] :: [CQSWrapper])
  acronyms --FIXME: All named ideas, not just acronyms.

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

glassBR_mg :: Document
glassBR_mg = mgDoc'' glassBRProg for mg_authors mgBod

this_symbols :: [QSWrapper]
this_symbols = ((map qs glassBRSymbols) ++ (map qs glassBRUnitless))

s2 = SRS.intro [Con s2_intro, Sub s2_1, Sub s2_2, Sub s2_3]

s2_intro = Paragraph $ 
  S "Software is helpful to efficiently and correctly predict the" +:+ 
  (phrase $ blast ^. term) +:+ S "risk involved with the" +:+. 
  (phrase $ glaSlab ^. term) +:+ S "The" +:+ (phrase $ blast ^. term) 
  +:+ S "under consideration is" +:+. (sLower (blast ^. defn)) +:+ --FIXME: find a way to remove "sLower" entirely.
  S "The software, herein called" +:+ (gLassBR ^. defn) +:+ S "aims to" +:+
  S "predict the" +:+ (phrase $ blast ^. term) +:+ 
  S "risk involved with the" +:+ 
  (phrase $ glaSlab ^. term) +:+ S "using an intuitive" +:+
  S "interface. The following" +:+ phrase section_ +:+ S "provides an overview" +:+
  S "of the" +:+ titleize srs +:+ sParen (short srs) +:+ S "for" +:+. 
  (gLassBR ^. defn) +:+ S "This" +:+ phrase section_ +:+ S "explains the" +:+ 
  phrase purpose +:+ S "of the" +:+ phrase document +:+ S "is designed to fulfil, the" +:+ 
  phrase scope +:+ S "of the" +:+ plural requirement +:+ S "and" +:+ S "the" +:+ 
  phrase organization +:+ S "of the" +: phrase document +:+ S "what the" +:+ 
  phrase document +:+. S "is based on and intended to portray"

s2_1 = Section (titleize prpsOfDoc) (map Con s2_1_intro)

s2_1_intro = 
  [Paragraph $
  S "The main" +:+ phrase purpose +:+ S "of this" +:+ phrase document +:+
  S "is to predict whether a given" +:+ (phrase $ glaSlab ^. term) +:+
  S "is likely to resist a specified" +:+. (phrase $ blast ^. term) +:+
  S "The goals and" +:+ plural thModel +:+ S "used in the" +:+ (gLassBR ^. defn) +:+
  S "code are provided" `sC` S "with an emphasis on explicitly identifying" +:+ 
  (plural assumption) +:+ S "and unambiguous" +:+. plural definition +:+
  S "This" +:+ phrase document +:+ S "is intended to be used as a" +:+ phrase reference +:+
  S "to provide all" +:+ phrase information +:+ S "necessary to understand and" +:+
  S "verify the analysis. The" +:+ (short srs) +:+ S "is abstract" +:+
  S "because the contents say what" +:+ phrase problem +:+ S "is being solved, but not how" +:+.
  S "to solve it", Paragraph $ S "This" +:+ phrase document +:+ S "will be used" +:+
  S "as a starting point for subsequent development phases, including writing the" +:+
  phrase desSpec +:+ S "and the software verification and validation plan. The design" +:+
  phrase document +:+ S "will show how the" +:+ plural requirement +:+ 
  S "are to be realized, including" +:+ S "decisions on the numerical" +:+
  S "algorithms and programming environment. The verification and validation" +:+
  S "plan will show the steps that will be used to increase confidence in the" +:+.
  S "software documentation and the implementation"]

s2_2 = Section (S "Scope of" +:+ titleize' requirement) 
  [Con s2_2_intro]

s2_2_intro = Paragraph $
  S "The scope of the" +:+ plural requirement +:+
  S "includes getting all input parameters related to the" +:+ 
  (phrase $ glaSlab ^. term) +:+ S "and also the parameters" +:+
  S "related to" +:+. (phrase $ blastTy ^. term) +:+ 
  S "Given the input" `sC` (gLassBR ^. defn) +:+ S "is intended to" +:+
  S "use the data and predict whether the" +:+ 
  (phrase $ glaSlab ^. term) +:+. S "is safe to use or not"

s2_3 = orgSecWTS s2_3_intro dataDefn s6_2_4 s2_3_intro_end

s2_3_intro = 
  S "The" +:+ phrase organization +:+ S "of this" +:+ phrase document +:+ 
  S "follows the template for an" +:+ (short srs) +:+ S "for" +:+ 
  phrase sciCompS +:+ S "proposed by [1] and [2] (in" +:+ (makeRef s10) :+:
  S "), with some aspects taken from Volere template 16 [3]."
  
s2_3_intro_end = S "The" +:+ (plural dataDefn) +:+
  S "are used to support the" +:+ plural definition +:+ S "of the different"
  +:+ plural model
  
s3 = Section(S "Stakeholders") [Con s3_intro, Sub s3_1, Sub s3_2]

s3_intro = Paragraph $
  S "This" +:+ phrase section_ +:+ S "describes the Stakeholders: the" +:+.
  S "people who have an interest in the product"

s3_1 = Section (S "The Client") [Con s3_1_intro]

s3_1_intro = Paragraph $
  S "The client for" +:+ (gLassBR ^. defn) +:+ S "is a company named" +:+
  S "Entuitive. It is developed by Dr. Manuel Campidelli. The client has" +:+.
  S "the final say on acceptance of the product"

s3_2 = Section (S "The Customer") [Con s3_2_intro]

s3_2_intro = Paragraph $
  S "The customers are the end user of" +:+. (gLassBR ^. defn)

s4 = Section(titleize generalSystemDescription) [Con s4_intro, Sub s4_1, 
  Sub s4_2]

s4_intro = Paragraph $
  S "This" +:+ phrase section_ +:+ S "provides" +:+ phrase general +:+ 
  phrase information +:+ S "about the" +:+ phrase system `sC` S "identifies the interface" +:+
  S "between the" +:+ phrase system +:+ S "and its environment" `sC`
  S "and describes the user characteristics and the" +:+. plural systemConstraint

s4_1 = Section (S "User Characteristics") [Con s4_1_bullets]

s4_1_bullets = Enumeration $ Bullet $ map Flat
  [(S "The end user of" +:+ (gLassBR ^. defn) +:+ S "is expected to" +:+
  S "have completed at least the equivalent of the second year of an" +:+.
  S "undergraduate degree in civil or structural engineering"),
  (S "The end user is expected to have an understanding of" +:+ phrase theory +:+
  S "behind" +:+ (phrase $ gbr ^. term) +:+ S "and" +:+ (phrase $ blast ^. term) +:+.
  S "risk"), (S "The end user is expected to have basic computer literacy to handle" +:+.
  S "the software")]

s4_2 = Section (titleize' systemConstraint) [Con s4_2_intro]

s4_2_intro = Paragraph $
  (short notApp)

s5 = Section(S "Scope of the Project") [Con s5_intro, Sub s5_1, Sub s5_2]

s5_intro = Paragraph $
  S "This" +:+ phrase section_ +:+ S "presents the scope of the project. It" +:+
  S "describes the expected use of" +:+ (gLassBR ^. defn) +:+ S "as well as the" +:+
  S "inputs and outputs of each action. The use cases are input and" +:+
  S "output, which defines the action of getting the input and displaying" +:+.
  S "the output"

s5_1 = Section (S "Product Use Case Table") [Con s5_1_table]

s5_1_table = Table [S "Use Case NO.", S "Use Case Name", S "Actor", 
  S "Input and Output"] (mkTable
  [(\x -> (x!!0)),(\x -> (x!!1)), (\x -> (x!!2)), (\x -> (x!!3))]
  [[S "1", S "Inputs", S "User", titleize' characteristic +:+ S "of the" +:+
  (phrase $ glaSlab ^. term) +:+ S "and of the" +:+.
  (phrase $ blast ^. term) +:+ S "Details in" +:+ 
  (makeRef s5_2)],
  [S "2", S "Output", (gLassBR ^. defn), S "Whether or not the" +:+
  (phrase $ glaSlab ^. term) +:+ S "is safe for the calculated" +:+
  (phrase $ load ^. term) +:+ S "and supporting" +:+
  S "calculated values"]])
  (titleize table_ +:+ S "1: Use Case Table") True

s5_2 = Section (S "Individual Product Use Cases") [Con s5_2_bullets]

s5_2_bullets = Enumeration $ Bullet $ map Flat
  [(S "Use Case 1 refers to the user providing input to" +:+ 
  (gLassBR ^. defn) +:+ S "for use within the analysis. There are two" +:+
  S "classes of inputs:" +:+ (phrase $ glassGeo ^. term) +:+
  S "and" +:+. (phrase $ blastTy ^. term) +:+
  (glassGeo ^. defn) +:+ (blastTy ^. defn) +:+ S "These" +:+
  S "parameters describe" +:+ (phrase $ char_weight ^. term) +:+
  S "and stand off" +:+. (phrase $ blast ^. term) +:+
  S "Another input the user gives is the tolerable value of" +:+.
  (phrase $ prob_br ^. term)),
  (S " Use Case 2" +:+ (gLassBR ^. defn) +:+ S "outputs if the" +:+
  (phrase $ glaSlab ^. term) +:+ S "will be safe by" +:+
  S "comparing whether" +:+ (phrase $ capacity ^. term) +:+
  S "is greater than" +:+. (phrase $ demandq ^. term) +:+
  (at_start $ capacity ^. term) +:+ S "is the" +:+
  (sLower (capacity ^. defn)) +:+ S "and" +:+
  (phrase $ demandq ^. term) +:+ S "is the" +:+ phrase requirement +:+
  S "which is the" +:+. (demandq ^. defn) +:+ S "The second" +:+ 
  phrase condition +:+ S "is to check whether the calculated probability" +:+ 
  sParen (P $ prob_br ^. symbol) +:+ S "is less than the tolerable probability" +:+ 
  sParen (P $ pb_tol ^. symbol) +:+ S "which is obtained from the user" +:+
  S "as an input. If both" +:+ plural condition +:+ S "return true then it's shown that the" 
  +:+ (phrase $ glaSlab ^. term) +:+ S "is safe to use" `sC` 
  S "else if both return false then the" +:+ 
  (phrase $ glaSlab ^. term) +:+. S "is considered unsafe" +:+.
  S "All the supporting calculated values are also displayed as output")]

s6 = Section(titleize specificsystemdescription) [Con s6_intro, Sub s6_1,
  Sub s6_2]

s6_intro = Paragraph $ 
  S "This" +:+ phrase section_ +:+ S "first presents the" +:+ phrase problemDescription `sC` S "which gives a" +:+
  S "high-level view of the" +:+ phrase problem +:+ S "to be solved. This is followed by the" +:+
  phrase solution +:+ phrase characteristicsSpecification :+: S ", which presents the" +:+
  (plural assumption) `sC` plural theory :+: S "," +:+. plural definition

s6_1 = Section (titleize problemDescription) [Con s6_1_intro, Sub s6_1_1, 
  Sub s6_1_2, Sub s6_1_3]

s6_1_intro = Paragraph $ 
  S "A" +:+ phrase system +:+ S "is needed to efficiently and correctly predict the"
  +:+ (phrase $ blast ^. term) +:+. S "risk involved with the glass" +:+ (gLassBR ^. defn)
  +:+ S "is a" +:+ S "computer program developed to interpret the inputs to give out the" +:+
  S "outputs which predicts whether the" +:+ (phrase $ glaSlab ^. term) +:+
  S "can withstand the" +:+ (phrase $ blast ^. term) +:+ S "under the" +:+. plural condition

s6_1_1 = Section (titleize terminology +:+ S "and" +:+ titleize' definition) [Con s6_1_1_intro, 
  Con s6_1_1_bullets]
  
s6_1_1_intro = Paragraph $ 
  S "This subsection provides a list of terms that are used in subsequent" +:+
  plural section_ +:+ S "and their meaning, with the" +:+ phrase purpose +:+
  S "of reducing ambiguity and making it easier to correctly understand the" +:+. 
  (plural requirement) +:+ S "All of the terms" +:+
  S "are extracted from [4] in" +:+. (makeRef s10)

s6_1_1_bullets = Enumeration $ (Number $ 
  [Flat $ ((phrase $ aR ^. term) :+: sParenDash (short aspectR)) :+: 
  (aR ^. defn)] ++
  map (\c -> Flat $ ((at_start $ c ^. term) +:+ S "- ") :+: (c ^. defn)) [gbr, lite] ++ 
  [Nested (((phrase $ glassTy ^. term) :+: S ":")) (Bullet $ map (\c -> Flat c)
  [(((phrase $ an ^. term) :+: sParenDash (short annealedGlass)) :+: 
    (an ^. defn)),
  (((phrase $ ft ^. term) :+: sParenDash (short fullyTGlass)) :+:
    (ft ^. defn)),
  (((phrase $ hs ^. term) :+: sParenDash (short heatSGlass)) :+:
    (hs ^. defn))])] ++
  map (\c -> Flat c)
  [(((phrase $ gtf ^. term) :+: sParenDash (short glassTypeFac)) :+: 
  (gtf ^. defn)),
  (((at_start $ lateral ^. term) +:+ S "- ") :+: (lateral ^. defn))] ++ 
  [Nested (((at_start $ load ^. term) :+: S ":")) (Bullet $ map (\c -> Flat c)  
  [(((phrase $ specDeLoad ^. term) +:+ S "- ") :+: (specDeLoad ^. defn)),
  (((phrase $ lr ^. term) :+: sParenDash (short lResistance)) :+: --lr and lResistance should be the same concepts
    (lr ^. defn)),
  (((phrase $ ldl ^. term) +:+ S "- ") :+: (ldl ^. defn)),
  (((phrase $ nfl ^. term) :+: sParenDash (short nonFactorL)) :+: --Same for nfl and nonFactorL
    (nfl ^. defn))] ++ 
  map (\c -> Flat $ ((phrase $ c ^. term) +:+ S "- ") :+: (c ^. defn))
    [glassWL, sdl])] ++ 
  map (\c -> Flat c)
  [(((phrase $ lsf ^. term) :+: sParenDash (short lShareFac)) :+: 
  (lsf ^. defn)),
  (((phrase $ pb ^. term) :+: sParenDash (P $ prob_br ^. symbol)) :+:
  (pb ^. defn))] ++
  map (\c -> Flat $ ((phrase $ c ^. term) +:+ S "- ") :+: (c ^. defn)) 
  [specA, blaReGLa, eqTNTChar] ++
  [Flat $ ((at_start $ sD ^. term) :+: sParenDash (P $ sd ^. symbol)) :+:
  (sD ^. defn)])
  where sParenDash = \x -> S " (" :+: x :+: S ") - "
  
s6_1_2 = Section (titleize physSyst) [Con s6_1_2_intro, Con s6_1_2_list, 
  Con fig_glassbr]

s6_1_2_intro = Paragraph $ S "The" +:+ phrase physicalSystem +:+ S "of" +:+ (gLassBR ^. defn) 
  +:+ S "as shown in" +:+ (makeRef fig_glassbr) `sC` S "includes the" +:+
  S "following elements:"

fig_glassbr = Figure (S "The" +:+ phrase physicalSystem) "physicalsystimage.png"
  
s6_1_2_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (((short physSyst) :+: S "1"), (at_start $ glaSlab ^. term)), 
  (((short physSyst) :+: S "2"), S "The point of explosion." +:+
  S "Where the bomb, or" +:+ sLower (blast ^. defn) `sC` 
  S "is located. The" +:+ ((phrase $ sD ^. term)) 
  +:+. S "is the distance between the point of explosion and the glass")]
--NOTE: The only difference here from the original is the removal of an 
--    extraneous space

s6_1_3 = Section (titleize' goalStmt) [Con s6_1_3_list]

s6_1_3_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (((short goalStmt) :+: S "1"), S "Analyze and predict whether the" +:+
  (phrase $ glaSlab ^. term) +:+ S "under consideration" +:+
  S "will be able to withstand the explosion of a certain degree which" +:+.
  S "is calculated based on user input")]

s6_2 = Section (titleize solution +:+ titleize characteristicsSpecification) 
  [Con s6_2_intro, Sub s6_2_1, Sub s6_2_2, Sub s6_2_3, Sub s6_2_4, Sub s6_2_5]

s6_2_intro = Paragraph $ S "This" +:+ phrase section_ +:+ S "explains all the" +:+
  (plural assumption) +:+ S "considered and the" +:+
  plural thModel +:+ S "which are" +:+
  S "supported by the" +:+. (plural dataDefn)
  
s6_2_1 = Section (titleize' assumption) ([Con s6_2_1_intro] ++
  (map Con s6_2_1_list))

s6_2_1_intro = Paragraph $ 
  S "This" +:+ phrase section_ +:+ S "simplifies the original" +:+ phrase problem +:+
  S "and helps in developing the" +:+ (phrase thModel) +:+ S "[" :+: (short thModel) :+:
  S "] by filling in the missing" +:+ phrase information +:+ S "for the" +:+.
  phrase physicalSystem +:+ S "The numbers given in the" +:+ 
  S "square brackets refer to the" +:+ (phrase dataDefn) +:+ S "[" :+:
  (short dataDefn) :+: S "], or" +:+ phrase inModel +:+ S "[" :+: 
  (short inModel) :+: S "], in which the respective" +:+ (phrase assumption) 
  +:+. S "is used"

s6_2_1_list = 
  [(Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (((short assumption) :+: S "1"), S "The standard E1300-09a for" +:+
    (phrase $ calculation ^. term) +:+ S "applies only to monolithic, laminated, or insulating" +:+
    S "glass constructions of rectangular shape with continuous" +:+ 
    (phrase $ lateral ^. term) +:+
    S "support along one, two, three, or four edges. This practice assumes" 
    +:+ S "that (1) the supported glass edges for two, three and four-sided" 
    +:+ S "support" +:+ plural condition +:+ S "are simply supported and free to slip in" +:+
    S "plane; (2) glass supported on two sides acts as a simply supported" 
    +:+. S "beam and (3) glass supported on one side acts as a cantilever"), 
  (((short assumption) :+: S "2"), S "This practice does not apply" 
    +:+ S "to any form of wired, patterned, etched, sandblasted, drilled" `sC`
    S "notched, or grooved glass with surface and edge treatments" +:+.
    S "that alter the glass strength"),
  (((short assumption) :+: S "3"), S "This" +:+ phrase system +:+
    S "only considers the external explosion scenario for its" +:+. (plural $ calculation ^. term)),
  (((short assumption) :+: S "4"), S "Standard values used for" +:+
    (phrase $ calculation ^. term) +:+ S "in" +:+ (gLassBR ^. defn) +:+ S "are:")]),
  (EqnBlock $ (C sflawParamM):=(Int 7)),
  (EqnBlock $ (C sflawParamK):=(Grouping (Dbl 2.86)):*(Int 10):^
    (Neg (Int 53))),
  (EqnBlock $ (C mod_elas):=(Grouping (Dbl 7.17)):*(Int 10):^(Int 7)),
  (EqnBlock $ (C load_dur):=(Int 3)),
  --  (Number $ map (\c -> Flat c) [
  --  (P $ sflawParamM ^. symbol) +:+ S "= 7" +:+ Sy (sflawParamM ^. unit), 
  --  (P $ sflawParamK ^. symbol) +:+ S "= 2.86 * 10^(-53)" +:+ Sy (sflawParamK ^. unit), 
  --  (P $ mod_elas ^. symbol) +:+ S "= 7.17 * 10^7" +:+ Sy (mod_elas ^. unit),
  --  (P $ load_dur ^. symbol) +:+ S "= 3" +:+ Sy (load_dur ^. unit)]))] ++
  (Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (((short assumption) :+: S "5"), S "Glass under consideration" +:+
    S "is assumed to be a single" +:+.
    (phrase $ lite ^. term) +:+ S "Hence the value of" +:+ 
    (P $ loadSF ^. symbol) +:+ S "is equal to 1 for all" +:+ (plural $ calculation ^. term) +:+ S "in" 
    +:+. (gLassBR ^. defn)),
  (((short assumption) :+: S "6"), S "Boundary" +:+ plural condition +:+
    S "for the" +:+ (phrase $ glaSlab ^. term) +:+ S "is assumed to be 4-sided"
    +:+ S "support for" +:+. (plural $ calculation ^. term)),
  (((short assumption) :+: S "7"), S "The response type considered in" 
    +:+ (gLassBR ^. defn) +:+. S "is flexural"),
  (((short assumption) :+: S "8"), S "With" +:+ phrase reference +:+
    S "to A4 the value of" +:+ (phrase $ loadDF ^. term) +:+ 
    sParen (P $ loadDF ^. symbol) +:+ S "is a constant in" +:+. 
    (gLassBR ^. defn) +:+ S "It is calculated by the equation:" +:+
    --(P $ loadDF ^. symbol) +:+ S "=" +:+ (P $ load_dur ^. symbol) :+: 
    S ". Using this" `sC` (P $ loadDF ^. symbol) +:+. S "= 0.27")])]
  --equation in sentence

s6_2_2 = Section (titleize' thModel) (map Con s6_2_2_TMods)
  
s6_2_2_TMods :: [Contents]
s6_2_2_TMods = map Definition (map Theory tModels)

s6_2_3 = Section (titleize' inModel) (map Con s6_2_3_IMods)

s6_2_3_IMods :: [Contents]
s6_2_3_IMods = map Definition (map Theory iModels)

s6_2_4 = Section (titleize' dataDefn) 
  ((Con s6_2_4_intro):(map Con s6_2_4_DDefns))

s6_2_4_intro = Paragraph $ 
  S "This" +:+ phrase section_ +:+ S "collects and defines all the data needed to" +:+
  S "build the" +:+. plural inModel

s6_2_4_DDefns ::[Contents] 
s6_2_4_DDefns = map Definition (map Data dataDefns)

s6_2_5 = Section (S "Data Constraints") [Con s6_2_5_intro, --Con s6_2_5_table1, 
  Con s6_2_5_table2, Con s6_2_5_intro2] --, Con s6_2_5_table3]

s6_2_5_intro = Paragraph $
  titleize table_ +:+ S "2 (" :+: --(makeRef s6_2_5_table1) :+: 
  S ") shows the data" +:+
  S "constraints on the input variables. The column of" +:+ phrase physical +:+
  S "constraints gives the" +:+ phrase physical +:+ S "limitations on the range" +:+
  S "of values that can  be taken by the variable. The" +:+ plural constraint_ +:+  --supposed to have double space midsentence?
  S "are conservative, to give" +:+ S "the user of the" +:+ phrase model +:+ 
  S "the flexibility to experiment with unusual situations. The column of" +:+.
  S "typical values is intended to provide a feel for a common scenario" +:+
  S "The uncertainty column provides an" +:+
  S "estimate of the confidence with which the" +:+ phrase physical +:+ plural quantity +:+
  S"can be measured. This" +:+ phrase information +:+ S "would be part of the input if one were"
  +:+. S "performing an uncertainty quantification exercise" +:+ at_start table_ +:+ S "3 (" :+:
  (makeRef s6_2_5_table2) :+: S ") gives the values of the specification" +:+
  S "parameters used in" +:+ titleize table_ +:+ S "2 (" :+: --(makeRef s6_2_5_table1) :+: 
  S ")." +:+ 
  (P $ ar_max ^. symbol) +:+ S "refers to the" +:+
  (phrase $ ar_max ^. term) +:+. S "for the plate of glass"

-- s6_2_5_table1 = Table [S "Var", S "Physical Cons", S "Software Constraints", S "Typical Value",
--  S "Uncertainty"] (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3),
--  (\x -> x!!4)] [[(P $ plate_len ^. symbol), (P $ plate_len ^. symbol) +:+ S "> 0 and" +:+ 
--  (P $ plate_len ^. symbol) :+: S "/" :+: (P $ plate_width ^. symbol) +:+ S "> 1",
--  (P $ dim_min ^. symbol) +:+ S "<=" +:+ (P $ plate_len ^. symbol) +:+ S "<=" +:+ 
--  (P $ dim_max ^. symbol) +:+ S "and" +:+ (P $ plate_len ^. symbol) :+: S "/" :+: 
--  (P $ plate_width ^. symbol) +:+ S "<" +:+ (P $ ar_max ^. symbol), S "1500" +:+
--  Sy (plate_len ^. unit), S "10%"], [(P $ plate_width ^. symbol), (P $ (plate_width ^. symbol)) 
--  +:+ S "> 0 and" +:+ (P $ plate_width ^. symbol) +:+ S "<" +:+ (P $ plate_len ^. symbol),
--  (P $ dim_min ^. symbol) +:+ S "<=" +:+ (P $ plate_width ^. symbol) +:+ S "<=" +:+ 
--  (P $ dim_max ^.symbol) +:+ S "and" +:+ (P $ plate_len ^. symbol) :+: S "/" :+: 
--  (P $ plate_width ^. symbol) +:+ S "<" +:+ (P $ ar_max ^. symbol), S "1200" +:+ 
--  Sy (plate_width ^. unit), S "10%"], [(P $ pb_tol ^. symbol), S "0 <" +:+ 
--  (P $ pb_tol ^. symbol) +:+ S "< 1", S "-", S "0.008", S "0.1%"], [(P $ char_weight ^. symbol), 
--  (P $ char_weight ^. symbol) +:+ S ">= 0", (P $ cWeightMin ^. symbol) +:+ S "<" +:+ 
--  (P $ char_weight ^. symbol) +:+ S "<" +:+ (P $ cWeightMax ^. symbol), S "42" +:+ 
--  Sy (char_weight ^. unit), S "10%"],[(P $ tNT ^. symbol), (P $ tNT ^. symbol) :+: 
--  S " > 0", S "-", S "1", S "10%"], [(P $ sd ^. symbol), (P $ sd ^. symbol) +:+ S "> 0", 
--  (P $ sd_min ^. symbol) +:+ S "<" +:+ (P $ sd ^. symbol) +:+ S "<" +:+ 
--  (P $ sd_max ^. symbol), S "45" :+: Sy (sd ^. unit), S "10%"]])
--  (S "Table 2: Input Variables") True

s6_2_5_table2 = Table [S "Var", S "Value"] (mkTable 
  [(\x -> P $ fst x), (\x -> snd x)] 
  [(dim_min ^. symbol, S "0.1" +:+ Sy (unit_symb sd)), 
  (dim_max ^.symbol, S "0.1" +:+ Sy (unit_symb sd)),(ar_max ^. symbol, S "5"), 
  (cWeightMin ^. symbol, S "4.5" +:+ Sy (unit_symb cWeightMin)),
  (cWeightMax ^. symbol, S "910" +:+ Sy (unit_symb cWeightMax)), 
  (sd_min ^. symbol, S "6" +:+ Sy (unit_symb sd_min)), 
  (sd_max ^. symbol, S "130" +:+ Sy (unit_symb sd_max))])
  (titleize table_ +:+ S "3:" +:+ titleize specification +:+ S "Parameter Values") True

s6_2_5_intro2 = Paragraph $
  titleize table_ +:+ S "4 (" :+: --(makeRef s6_2_5_table3) :+:
  S ") shows the" +:+ plural constraint_
  +:+. S "that must be satisfied by the output"

-- s6_2_5_table3 = Table [S "Var", S "Physical Constraints"] (mkTable 
--  [(\x -> P $ fst(x)), (\x -> snd(x))] 
--  [(prob_br ^. symbol, S "0 <" +:+ (P $ prob_br ^. symbol) +:+ S "< 1")])
--  (S "Table4: Output Variables") True

s7 = Section (titleize' requirement) [Sub s7_1, Sub s7_2]

s7_1 = Section (S "Functional" +:+ titleize' requirement) 
  ([Con s7_1_intro] ++ (map Con s7_1_list))

s7_1_intro = Paragraph $
  S "The following" +:+ phrase section_+:+ S "provides the functional" +:+
  plural requirement `sC` S "the business tasks that the software" +:+.
  S "is expected to complete"

s7_1_list = 
  [(Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(((short requirement) :+: S "1"), S "Input the following" +:+
    plural quantity :+: S ", which define the glass dimensions" `sC` 
    (sLower (glassTy ^. defn)) `sC` S "tolerable probability"
    +:+ S "of failure and the" +:+ plural characteristic +:+ S "of the" +:
    (phrase $ blast ^. term))]),
  (table ((map qs [plate_len,plate_width,sdx,sdy,sdz,nom_thick,char_weight]) 
  ++ (map qs [glass_type,pb_tol,tNT])) (\x -> phrase $ x ^.term)),
--s7_1_table = Table [S "Symbol", S "Units", S "Description"] (mkTable
--  [(\ch -> P (ch ^. symbol)),  
--   (\ch -> maybeUnits $ ch ^. unit'),
--   (\ch -> ch ^. term)
--   ]
--  [plate_len,plate_width,glass_type,pb_tol,sdx,sdy,sdz,nom_thick,tNT,
--  char_weight])
--  (S "Input Parameters") False
  (Enumeration $ Simple $
  [(((short requirement) :+: S "2"), Nested (S "The" +:+ phrase system +:+
  S "shall set the known values as follows: ") (Bullet $ map (\c -> Flat c) 
    [(P $ sflawParamM ^. symbol) `sC` (P $ sflawParamK ^. symbol) `sC` 
    (P $ mod_elas ^. symbol) `sC` (P $ load_dur ^. symbol) +:+ 
    S "following" +:+ (short assumption) :+: S "4",
    (P $ loadDF ^. symbol) +:+ S "following" +:+ (short assumption) 
    :+: S "8",
    (P $ loadSF ^. symbol) +:+ S "following" +:+ (short assumption) 
    :+: S "5"]))] ++
  map (\(a,b) -> (a, Flat b))
  [(((short requirement) :+: S "3"), S "The" +:+ phrase system +:+ S "shall check" +:+
  S "the entered input values to ensure that they do not exceed the data" +:+
  S "constraints mentioned in" +:+. (makeRef s6_2_5) +:+ S "If any of" +:+
  S "the input parameters is out of bounds, an error message is" +:+
  S "displayed and the" +:+ (plural $ calculation ^. term) +:+. S "stop"),
  (((short requirement) :+: S "4"), S "Output the input" +:+ plural quantity +:+
  S "from" +:+ (short requirement) :+: S "1 and the known" +:+ plural quantity
  +:+ S "from" +:+ (short requirement) :+: S "2."),
  (((short requirement) :+: S "5"), S "If" +:+ (P $ is_safe1 ^. symbol)
  +:+ S "and" +:+ (P $ is_safe2 ^. symbol) +:+ S "(from" +:+ 
  (makeRef (Definition (Theory t1SafetyReq))) +:+ S "and" +:+ 
  (makeRef (Definition (Theory t2SafetyReq))) :+: S ") are true" `sC`
  S "output the message" +:+ Quote (safeMessage ^. defn) +:+ S "If" +:+
  S "the" +:+ phrase condition +:+ S "is false, then output the message" +:+ 
  Quote (notSafe ^. defn))] ++
  [(((short requirement) :+: S "6"), Nested (S "Output the following" +:
  plural quantity)
  (Bullet $ 
    [Flat $ (at_start $ prob_br ^. term) +:+ sParen (P $ prob_br ^. symbol) +:+ 
    sParen (makeRef (Definition (Theory probOfBr)))] ++
    [Flat $ (phrase $ lRe ^. term) +:+ sParen(P $ lRe ^. symbol) +:+ 
    sParen (makeRef (Definition (Theory calOfCap)))] ++
    [Flat $ (phrase $ demand ^. term) +:+ sParen (P $ demand ^. symbol) +:+
    sParen (makeRef (Definition (Theory calOfDe)))] ++
    [Flat $ (phrase $ act_thick ^. term) +:+ sParen(P $ act_thick ^. symbol) +:+
    sParen (makeRef (Definition (Data hFromt)))] ++
    [Flat $ (titleize $ loadDF ^. term) +:+ sParen (P $ loadDF ^. symbol) +:+ 
    sParen (makeRef (Definition (Data loadDF)))]++
    [Flat $ (at_start $ strDisFac ^. term) +:+ sParen (P $ strDisFac ^. symbol) +:+ 
    sParen (makeRef (Definition (Data strDisFac)))]++
    [Flat $ (titleize $ nonFL ^. term) +:+ sParen (P $ nonFL ^. symbol) +:+ 
    sParen (makeRef (Definition (Data nonFL)))]++
    [Flat $ (phrase $ gTF ^. term) +:+ sParen(P $ gTF ^. symbol) +:+ 
    sParen (makeRef (Definition (Data glaTyFac)))] ++
    map (\c -> Flat $ (phrase $ c ^. term) +:+ sParen (P $ c ^. symbol) +:+ 
    sParen (makeRef (Definition (Data c))))
    [dL,tolPre,tolStrDisFac] ++
    [Flat $ (phrase $ ar ^. term) +:+ sParen(P $ ar ^. symbol)  
    --S " = a/b)"
    ]))])]

s7_2 = Section (S "Nonfunctional" +:+ titleize' requirement) 
  [Con s7_2_intro]

s7_2_intro = Paragraph $
  S "Given the small size, and relative simplicity, of this" +:+ phrase problem `sC`
  S "performance is not a priority. Any reasonable implementation will" +:+
  S "be very quick and use minimal storage. Rather than performance" `sC`
  S "the priority nonfunctional" +:+ (short requirement) :+: 
  S "s are correctness, verifiability, understandability, reusability," +:+.
  S "maintainability and portability"

s8 = Section(titleize' likelyChg) [Con s8_list]

s8_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(((short likelyChg) :+: S "1"), ((short assumption) :+: 
  S "3 - The" +:+ phrase system +:+ S "currently only calculates for external" +:+
  (phrase $ blast ^. term) +:+. S "risk" +:+.
  (S "In the future" +:+ (plural $ calculation ^. term) +:+ S "can be added for the internal" +:+
  (phrase $ blast ^. term) +:+ S "risk"))),
  (((short likelyChg) :+: S "2"), ((short assumption) :+:
  S "4" `sC` (short assumption) :+: S "8 - Currently the values for"
  +:+ (P $ sflawParamM ^. symbol) `sC` (P $ sflawParamK ^. symbol) `sC`
  S "and" +:+ (P $ mod_elas ^. symbol) +:+ S "are assumed to be the"
  +:+ S "same for all glass. In the future these values can be changed to"
  +:+. S "variable inputs")),
  (((short likelyChg) :+: S "3"), ((short assumption ) :+: 
  S "5 - The software may be changed to accommodate more than a single" +:+.
  (phrase $ lite ^. term))),
  (((short likelyChg) :+: S "4"), ((short assumption) :+: 
  S "6 - The software may be changed to accommodate more boundary" +:+
  plural condition +:+. S "than 4-sided support")),
  (((short likelyChg) :+: S "5"), ((short assumption) :+: 
  S "7 - The software may be changed to consider more than just flexure" +:+.
  S "of the glass"))]

s9 = Section((titleize' $ traceyMatrix ^. term) +:+ S "and" +:+ (titleize' $ graph ^. term))
  ([Con s9_intro1, Con s9_table1, Con s9_table2, Con s9_table3] ++ 
  (map Con s9_intro2) ++ 
  [Con fig_2, Con fig_3, Con fig_4])

s9_intro1 = Paragraph $
  S "The" +:+ phrase purpose +:+ S "of the" +:+ (plural $ traceyMatrix ^. term) +:+
  S "is to provide easy" +:+ plural reference +:+ S "on what has to be additionally" +:+
  S "modified if a certain component is changed. Every time a component is changed, the" +:+
  S "items in the column of that component that are marked with an" +:+ Quote (S "X") +:+.
  S "should be modified as well" +:+ at_start table_ +:+ S "5" +:+ 
  sParen (makeRef s9_table1) +:+ S "shows the" +:+ plural dependency +:+ S "of" +:+
  plural thModel `sC` (plural dataDefn) +:+ S "and" +:+ plural inModel +:+. S "with each other" +:+
  S "Table 6" +:+ sParen (makeRef s9_table2) +:+ S "shows the" +:+ plural dependency +:+ S "of" +:+
  plural requirement +:+ S "on" +:+ 
  plural thModel `sC`
  (plural inModel) `sC`
  (plural dataDefn) +:+. S "and data constraints" +:+
  S "Table 7" +:+ sParen (makeRef s9_table3) +:+ S "shows the" +:+ plural dependency +:+ S "of" +:+
  plural thModel `sC`
  (plural dataDefn) `sC`
  plural inModel `sC`
  plural likelyChg +:+ S "and" +:+
  (plural requirement) +:+ S "on the" +:+.
  (plural assumption)

--FIXME: There has to be a better way to do this.
s9_table1 = Table [EmptyS, 
  S "T1"  +:+ sParen (makeRef (Definition (Theory t1SafetyReq))), 
  S "T2"  +:+ sParen (makeRef (Definition (Theory t2SafetyReq))),
  S "IM1" +:+ sParen (makeRef (Definition (Theory probOfBr))), 
  S "IM2" +:+ sParen (makeRef (Definition (Theory calOfCap))),
  S "IM3" +:+ sParen (makeRef (Definition (Theory calOfDe))),
  S "DD1" +:+ sParen (makeRef (Definition (Data risk))),
  S "DD2" +:+ sParen (makeRef (Definition (Data hFromt))),
  S "DD3" +:+ sParen (makeRef (Definition (Data loadDF))), 
  S "DD4" +:+ sParen (makeRef (Definition (Data strDisFac))), 
  S "DD5" +:+ sParen (makeRef (Definition (Data nonFL))),
  S "DD6" +:+ sParen (makeRef (Definition (Data glaTyFac))),
  S "DD7" +:+ sParen (makeRef (Definition (Data dL))), 
  S "DD8" +:+ sParen (makeRef (Definition (Data tolPre))),
  S "DD9" +:+ sParen (makeRef (Definition (Data tolStrDisFac)))]
  --For now, I'm not propagating the sParen changes 
  --through the rest of this.
  [[S "T1 (" :+: (makeRef (Definition (Theory t1SafetyReq))) :+: S ")", EmptyS,
  S "X", S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS], 
  [S "T2 (" :+: (makeRef (Definition (Theory t2SafetyReq))) :+: S ")", S "X",
  EmptyS, EmptyS, S "X", S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, 
  EmptyS],
  [S "IM1 (" :+: (makeRef (Definition (Theory probOfBr))) :+: S ")", EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, S "X", S "X", S "X", S "X", EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS],
  [S "IM2 (" :+: (makeRef (Definition (Theory calOfCap))) :+: S ")", EmptyS, 
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X", S "X", EmptyS, EmptyS,
  EmptyS],
  [S "IM3 (" :+: (makeRef (Definition (Theory calOfDe))) :+: S ")", EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS],
  [S "DD1 (" :+: (makeRef (Definition (Data risk))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD2 (" :+: (makeRef (Definition (Data hFromt))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD3 (" :+: (makeRef (Definition (Data loadDF))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD4 (" :+: (makeRef (Definition (Data strDisFac))) :+: S ")", EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS,
  EmptyS],
  [S "DD5 (" :+: (makeRef (Definition (Data nonFL))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS],
  [S "DD6 (" :+: (makeRef (Definition (Data glaTyFac))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD7 (" :+: (makeRef (Definition (Data dL))) :+: S ")", EmptyS, EmptyS, EmptyS,
  EmptyS, S "X", EmptyS, S "X", EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS],
  [S "DD8 (" :+: (makeRef (Definition (Data tolPre))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X"],
  [S "DD9 (" :+: (makeRef (Definition (Data tolStrDisFac))) :+: S ")", EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X", S "X", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS]]
  ((titleize $ traceyMatrix ^. term) +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between Items of Different" +:+ titleize' section_) True

-- FIXME: Same goes for this one (see above)
s9_table2 = Table [EmptyS, S "T1 (" :+: 
  (makeRef (Definition (Theory t1SafetyReq))) :+: S ")", S "T2 (" :+: 
  (makeRef (Definition (Theory t2SafetyReq))) :+: S ")", S "IM1 (" :+:
  (makeRef (Definition (Theory probOfBr))) :+: S ")", S "IM2 (" :+:
  (makeRef (Definition (Theory calOfCap))) :+: S ")", S "IM3 (" :+:
  (makeRef (Definition (Theory calOfDe))) :+: S ")", S "DD1 (" :+:
  (makeRef (Definition (Data risk))) :+: S ")", S "DD2 (" :+:
  (makeRef (Definition (Data hFromt))) :+: S ")", S "DD3 (" :+:
  (makeRef (Definition (Data loadDF))) :+: S ")", S "DD4 (" :+:
  (makeRef (Definition (Data strDisFac))) :+: S ")", S "DD5 (" :+:
  (makeRef (Definition (Data nonFL))) :+: S ")", S "DD6 (" :+:
  (makeRef (Definition (Data glaTyFac))) :+: S ")", S "DD7 (" :+:
  (makeRef (Definition (Data dL))) :+: S ")", S "DD8 (" :+:
  (makeRef (Definition (Data tolPre))) :+: S ")", S "DD9 (" :+:
  (makeRef (Definition (Data tolStrDisFac))) :+: S ")", S "Data Constraints (" 
  :+: (makeRef s6_2_5) :+: S ")", S "R1 (in" +:+ (makeRef s7_1) :+: S ")",
  S "R2 (in" +:+ (makeRef s7_1) :+: S ")"]
  [[S "R1 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "R2 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "R3 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS],
  [S "R4 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, S "X", S "X"],
  [S "R5 (in" +:+ (makeRef s7_1) :+: S ")", S "X", S "X", EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "R6 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, S "X", S "X", S "X",
  EmptyS, S "X", S "X", S "X", S "X", S "X", S "X", S "X", S "X", EmptyS, EmptyS,
  EmptyS]]
  ((titleize $ traceyMatrix ^. term) +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ titleize' requirement +:+ S "and Other Items") True

-- FIXME: Same goes for this one (see above)
s9_table3 = Table [EmptyS, S "A1 (in" +:+ (makeRef s6_2_1) :+: S ")",
  S "A2 (in" +:+ (makeRef s6_2_1) :+: S ")", S "A3 (in" +:+ 
  (makeRef s6_2_1) :+: S ")", S "A4 (in" +:+ (makeRef s6_2_1) :+: S ")",
  S "A5 (in" +:+ (makeRef s6_2_1) :+: S ")", S "A6 (in" +:+
  (makeRef s6_2_1) :+: S ")", S "A7 (in" +:+ (makeRef s6_2_1) :+: S ")",
  S "A8 (in" +:+ (makeRef s6_2_1) :+: S ")"]
  [[S "T1 (" :+: (makeRef (Definition (Theory t1SafetyReq))) :+: S ")", EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "T2 (" :+: (makeRef (Definition (Theory t2SafetyReq))) :+: S ")", EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "IM1 (" :+: (makeRef (Definition (Theory probOfBr))) :+: S ")", EmptyS,
  EmptyS, EmptyS, S "X", EmptyS, S "X", S "X", EmptyS],
  [S "IM2 (" :+: (makeRef (Definition (Theory calOfCap))) :+: S ")", EmptyS,
  EmptyS, EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS],
  [S "IM3 (" :+: (makeRef (Definition (Theory calOfDe))) :+: S ")", EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD1 (" :+: (makeRef (Definition (Data risk))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD2 (" :+: (makeRef (Definition (Data hFromt))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD3 (" :+: (makeRef (Definition (Data loadDF))) :+: S ")", EmptyS, EmptyS,
  EmptyS, S "X", EmptyS, EmptyS, EmptyS, S "X"],
  [S "DD4 (" :+: (makeRef (Definition (Data strDisFac))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD5 (" :+: (makeRef (Definition (Data nonFL))) :+: S ")", EmptyS, EmptyS,
  EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD6 (" :+: (makeRef (Definition (Data glaTyFac))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD7 (" :+: (makeRef (Definition (Data dL))) :+: S ")", EmptyS, EmptyS, EmptyS,
  EmptyS, S "X", EmptyS, EmptyS, EmptyS],
  [S "DD8 (" :+: (makeRef (Definition (Data tolPre))) :+: S ")", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS, EmptyS, EmptyS, EmptyS],
  [S "DD9 (" :+: (makeRef (Definition (Data tolStrDisFac))) :+: S ")", EmptyS,
  EmptyS, EmptyS, S "X", EmptyS, EmptyS, EmptyS, EmptyS],
  [S "LC1 (in" +:+ (makeRef s8) :+: S ")", EmptyS, EmptyS, S "X", EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [S "LC2 (in" +:+ (makeRef s8) :+: S ")", EmptyS, EmptyS, EmptyS, S "X", EmptyS,
  EmptyS, EmptyS, S "X"],
  [S "LC3 (in" +:+ (makeRef s8) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, S "X",
  EmptyS, EmptyS, EmptyS],
  [S "LC4 (in" +:+ (makeRef s8) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  S "X", EmptyS, EmptyS],
  [S "LC5 (in" +:+ (makeRef s8) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, S "X", EmptyS],
  [S "R1 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [S "R2 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, EmptyS, S "X", S "X",
  EmptyS, EmptyS, S "X"],
  [S "R3 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [S "R4 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [S "R5 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS],
  [S "R6 (in" +:+ (makeRef s7_1) :+: S ")", EmptyS, EmptyS, EmptyS, EmptyS, EmptyS,
  EmptyS, EmptyS, EmptyS]]
  ((titleize $ traceyMatrix ^. term) +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ titleize' assumption +:+ S "and Other Items") True

s9_intro2 = 
  [Paragraph $
  S "The" +:+ phrase purpose +:+ S "of the traceability" +:+ (plural $ graph ^. term) 
  +:+ S "is also to provide easy" +:+ plural reference +:+ S "on what has to be" +:+
  S "additionally modified if a certain component is changed. The arrows in the" +:+ 
  (plural $ graph ^. term) +:+ S "represent" +:+. plural dependency +:+
  S "The component at the tail of an arrow is depended on" +:+
  S "by the component at the head of that arrow. Therefore, if a" +:+
  S "component is changed, the components that it points to should also" +:+.
  S "be changed" +:+ titleize figure +:+ S "2" +:+ sParen (makeRef fig_2) +:+ S "shows the" +:+
  plural dependency +:+ S "of" +:+ plural thModel `sC` (plural dataDefn) +:+ S "and" +:+
  plural inModel +:+. S "on each other" +:+
  titleize figure +:+ S "3" +:+ sParen (makeRef fig_3) +:+ S "shows the" +:+ plural dependency +:+
  S "of" +:+ plural requirement +:+ S "on" +:+
  plural thModel `sC` 
  plural inModel `sC`
  (plural dataDefn) +:+. S "and data constraints" +:+
  titleize figure +:+ S "4" +:+ sParen (makeRef fig_4) +:+ S "shows the" +:+ plural dependency +:+ 
  S "of" +:+ plural thModel `sC` 
  plural inModel `sC`
  (plural dataDefn) `sC` 
  plural requirement +:+ S "and" +:+
  (plural likelyChg) +:+ S "on" +:+.
  (plural assumption),
  Paragraph $ 
  S "NOTE: Building a tool to automatically generate the graphical" +:+
  S "representation of the" +:+ (phrase $ matrix ^. term) +:+ S "by scanning the" +:+
  S "labels and" +:+ phrase reference +:+. S "can be future work"]

fig_2 = Figure (titleize figure +:+ S "2:" +:+ (titleize $ traceyMatrix ^. term) 
  +:+ S "Showing the" +:+ titleize' connection +:+ S "Between Items of Different Sections") "Trace.png"

fig_3 = Figure (titleize figure +:+ S "3:" +:+ (titleize $ traceyMatrix ^. term) +:+ 
  S "Showing the" +:+ titleize' connection +:+ S "Between" +:+ (titleize' requirement) +:+
  S "and Other Items") "RTrace.png"

fig_4 = Figure (titleize figure +:+ S "4:" +:+ (titleize $ traceyMatrix ^. term) +:+
  S "Showing the" +:+ titleize' connection +:+ S "Between" +:+ (titleize' assumption) +:+
  S "and Other Items") "ATrace.png"

s10 = Section(titleize' reference) [Con s10_list]

s10_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(S "[1]", S "N. Koothoor" `sC` Quote (S "A" +:+ phrase document +:+ 
  S "drive approach to certifying" +:+ phrase sciCompS :+: S ",") +:+ S "Master's thesis"
  `sC` S "McMaster University, Hamilton, Ontario, Canada, 2013."),
  (S "[2]", S "W. S. Smith and L. Lai" `sC` Quote (S "A new" +:+ plural requirement +:+
  S "template for scientific computing,") +:+ S "in Proceedings of the" +:+
  S "First International Workshop on Situational" +:+ titleize' requirement +:+ 
  S "Engineering Processes - Methods, Techniques and Tools to Support Situation-Specific" +:+
  titleize' requirement +:+ S "Engineering Processes, SREP'05 (J.Ralyt" :+: 
  (F Acute 'e') :+: S ", P.Agerfalk, and N.Kraiem, eds.), (Paris, France),"
  +:+ S "pp. 107-121, In conjunction with 13th IEEE International" +:+
  titleize' requirement +:+. S "Engineering Conference, 2005"),
  (S "[3]", S "J. Robertson and S. Robertson" `sC` Quote (S "Volere" +:+
  plural requirement +:+ phrase specification +:+. S "template edition 16") +:+ 
  Quote (S "www.cs.uic.edu/ i442/VolereMaterials/templateArchive16/c" +:+ 
  S "Volere template16.pdf") :+: S ", 2012."),
  (S "[4]", S "ASTM Standards Committee" `sC` Quote (S "Standard practice"
  +:+ S "for determining" +:+ (phrase $ load ^. term) +:+ S "resistance of" +:+
  S "glass in buildings,") :+: 
  S " Standard E1300-09a, American Society for Testing and Material (ASTM),"
  +:+. S "2009"),
  (S "[5]", S "ASTM, developed by subcommittee C1408,Book of standards 15.02,"
  +:+ Quote (S "Standard" +:+ phrase specification +:+. S "for flat glass,C1036")),
  (S "[6]", S "ASTM, developed by subcommittee C14.08,Book of standards" +:+
  S "15.02" `sC` Quote (at_start specification +:+ S "for heat treated flat glass-Kind"
  +:+. S "HS, kind FT coated and uncoated glass,C1048"))]

s11 = Section(titleize appendix) [Con s11_intro, Con fig_5, Con fig_6]

s11_intro = Paragraph $
  S "This" +:+ phrase appendix +:+ S "holds the" +:+ (plural $ graph ^. term) +:+ sParen ((makeRef fig_5)
  +:+ S "and" +:+ (makeRef fig_6)) +:+ S "used for interpolating values needed in the"
  +:+. plural model

fig_5 = Figure (titleize figure +:+ S "5:" +:+ (demandq ^. defn) +:+ sParen
  (P (demand ^. symbol)) +:+ S "versus" +:+ (at_start $ sD ^. term) +:+
  S "versus" +:+ (at_start $ char_weight ^. term) +:+ sParen
  (P (sflawParamM ^. symbol))) "ASTM_F2248-09.png"

fig_6 = Figure (titleize figure +:+ S "6: Non dimensional" +:+ 
  (phrase $ lateral ^. term) +:+
  (phrase $ load ^. term) +:+ sParen
  (P (dimlessLoad ^. symbol)) +:+ S "versus" +:+ (phrase $ ar ^. term) +:+ 
  sParen (P (ar ^. symbol)) +:+ S "versus" +:+ (phrase $ sdf ^. term) +:+ 
  sParen (P (sdf ^. symbol))) "ASTM_F2248-09_BeasonEtAl.png"

