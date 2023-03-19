module Drasil.BmBnd.SRS where

import           Drasil.DocLang
import           Drasil.SRSDocument
import           Language.Drasil

-- FIXME: I added this argument for the SRSDecl, but I don't think it should be
-- needed. This is temporary work to avoid the cyclic dependency shown below.
srsBody :: SystemInformation
        -> CI
        -> LabelledContent
        -> [ConceptChunk]
        -> [ConstQDef]
        -> SRSDecl
srsBody si cs psdFig terms constQDs =
  [ TableOfContents
  , RefSec $ RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA]
  , IntroSec
    $ IntroProg (S "") (S "") [IPurpose [], IScope (S ""), IChar [] [] []]
       -- IOrgSec
  , GSDSec
    $ GSDProg [SysCntxt [], UsrChars [userCharacteristics], SystCons [] []]
  , SSDSec
    $ SSDProg
      [ SSDProblem
        $ PDProg
          (S "")
          []
          [ TermsAndDefs Nothing terms
          , PhySysDesc cs [] psdFig []
          , Goals [S ""]]
      , SSDSolChSpec
        $ SCSProg
          [ Assumptions
          , TMs [] (Label:stdFields)
          , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
          , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
          , IMs
              []
              ([Label, Input, Output, InConstraints, OutConstraints]
               ++ stdFields)
              ShowDerivation]]
  , ReqrmntSec $ ReqsProg [FReqsSub EmptyS [], NonFReqsSub]
  , LCsSec
  , UCsSec
  , TraceabilitySec $ TraceabilityProg $ traceMatStandard si  -- FIXME: the SRSDecl referencing the SystemInformation is akin to a cyclic dependency (talking about Drasil, not Haskell).
  , AuxConstntSec $ AuxConsProg cs constQDs -- FIXME: In a similar fashion to the above, I should not have to manually reference the case study nor the constants! It should be filled in for me.
  , Bibliography]

stdFields :: Fields
stdFields =
  [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

userCharacteristics :: Contents
userCharacteristics = foldlSP [S "Hello world!"]
