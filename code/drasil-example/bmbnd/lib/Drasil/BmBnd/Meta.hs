module Drasil.BmBnd.Meta where

-- FIXME: These below dependencies are all required for the main layout of the
-- SRS documents! I wish they were presuppositions instead.
import           Data.Drasil.Concepts.Computation
import qualified Data.Drasil.Concepts.Documentation as Doc
import           Data.Drasil.Concepts.Education
import qualified Data.Drasil.Concepts.Math as CM
import qualified Data.Drasil.Concepts.Physics as CP
import           Data.Drasil.Concepts.Software
import           Data.Drasil.SI_Units
import           Drasil.SRSDocument
import           Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import           Theory.Drasil
-- My 'real' imports.
import qualified Drasil.BmBnd.Assumptions as As
import qualified Drasil.BmBnd.Constants as Cs
import qualified Drasil.BmBnd.DataDefinitions as DDs
import qualified Drasil.BmBnd.Goals as Gs
import qualified Drasil.BmBnd.InstanceModels as IMs
import qualified Drasil.BmBnd.Quantities as Qs
import qualified Drasil.BmBnd.References as Rs
import           Drasil.BmBnd.SRS
import qualified Drasil.BmBnd.TheoryModels as TMs
import qualified Drasil.BmBnd.Figures as Fs
import qualified Drasil.BmBnd.Units as Us
import qualified Drasil.BmBnd.Terminology as Ts
import qualified Drasil.BmBnd.Changes.Likely as LCs
import qualified Drasil.BmBnd.Changes.Unlikely as UCs
import qualified Drasil.BmBnd.Requirements.Functional as FRs
import qualified Drasil.BmBnd.Requirements.NonFunctional as NFRs

caseStudy :: CI
caseStudy =
  commonIdeaWithDict "bmbnd" (pn "Beam Bending Analysis Program") "BmBnd" []

jasonBalaci :: Person
jasonBalaci = person "Jason" "Balaci"

purpose :: Sentence
purpose = S "To examine a beam bending under load."

si :: SystemInformation
si = SI { _sys = caseStudy
        , _kind = Doc.srs -- FIXME: ICO! The SystemInformation is approximately the SRS abstraction.
        , _authors = [jasonBalaci]
        , _background = []
        , _purpose = [purpose]
        , _quants = Qs.quantities -- FIXME: Remove!
        , _concepts = [] :: [DefinedQuantityDict] -- FIXME: Remove!
        , _instModels = IMs.models -- FIXME: Remove!
        , _datadefs = DDs.models -- FIXME: Remove!
        , _configFiles = [] -- FIXME: Remove!
        , _inputs = Qs.inputs -- FIXME: ICO!
        , _outputs = Qs.outputs -- FIXME: ICO!
        , _defSequence = [] :: [Block SimpleQDef] -- FIXME: ICO!
        , _constraints = [] :: [ConstrainedChunk] -- FIXME: Remove!
        , _constants = Cs.constants -- FIXME: Remove!
        , _sysinfodb = symbMap
        , _usedinfodb = usedDB -- FIXME: Remove!
        , refdb = refDB -- FIXME: Remove!
        }

terms :: [IdeaDict]
terms = nw caseStudy
  :(map nw Doc.doccon -- TODO: These items in the parentheses should be part of the 'fillCDB' background knowledge
    ++ map nw softwarecon
    ++ map nw Doc.doccon'
    ++ map nw CP.physicCon
    ++ map nw educon
    ++ [nw algorithm]
    ++ map nw derived
    ++ map nw fundamentals
    ++ map nw CM.mathcon
    ++ map nw CM.mathcon')
  ++ map nw Qs.quantities -- TODO: this is required, but feels like it should be done automatically, look into this
  ++ map nw Ts.terminology

conceptChunks :: [ConceptChunk]
conceptChunks = Doc.srsDomains

conceptInstances :: [ConceptInstance]
conceptInstances = As.assumptions
  ++ Gs.goals
  ++ LCs.changes
  ++ UCs.changes
  ++ FRs.requirements
  ++ NFRs.requirements

symbMap :: ChunkDB
symbMap = cdb
  Qs.quantities
  terms
  conceptChunks
  (map unitWrapper [metre, newton, pascal, radian]
   ++ Us.units) -- FIXME: shouldn't the SI units be presupposed?
  DDs.models
  IMs.models
  [] -- no general definitions
  TMs.models
  conceptInstances
  ([] :: [Section])
  Fs.figures -- FIXME: Even if I didn't put the 'figures' here, the SRS generator would still work fine, which means something isn't using the UID system appropriately.
  ([] :: [Reference])

usedDB :: ChunkDB
usedDB = cdb
  ([] :: [QuantityDict])
  [nw caseStudy]
  ([] :: [ConceptChunk])
  ([] :: [UnitDefn])
  ([] :: [DataDefinition])
  ([] :: [InstanceModel])
  ([] :: [GenDefn])
  ([] :: [TheoryModel])
  ([] :: [ConceptInstance])
  ([] :: [Section])
  ([] :: [LabelledContent])
  ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb Rs.references conceptInstances

realSrsBody :: SRSDecl -- FIXME: This is suboptimal.
realSrsBody = srsBody si caseStudy Fs.bmBndDiagram Ts.terminology Cs.constants

srs :: Document
srs = mkDoc realSrsBody (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS realSrsBody si