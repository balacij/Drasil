module Drasil.BmBnd.SRS where

import           Drasil.DocLang
import           Drasil.SRSDocument
import           Language.Drasil

-- FIXME: I added this argument for the SRSDecl, but I don't think it should be
-- needed. This is temporary work to avoid the cyclic dependency shown below.
srsBody :: SystemInformation -> SRSDecl
srsBody si =
  [ TableOfContents,
    RefSec $ RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA],
    IntroSec $ IntroProg (S "") (S "") [],
    GSDSec $ GSDProg [],
    SSDSec $ SSDProg [],
    ReqrmntSec $ ReqsProg [],
    LCsSec,
    TraceabilitySec $ TraceabilityProg $ traceMatStandard si, -- FIXME: the SRSDecl referencing the SystemInformation is akin to a cyclic dependency (talking about Drasil, not Haskell).
    Bibliography
  ]
