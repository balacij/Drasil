{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}
module Drasil.BmBnd.Meta where

-- FIXME: These below dependencies are all required for the main layout of the
-- SRS documents! I wish they were presuppositions instead.
import           Data.Drasil.Concepts.Computation
import qualified Data.Drasil.Concepts.Documentation      as Doc
import           Data.Drasil.Concepts.Education
import qualified Data.Drasil.Concepts.Math               as CM
import qualified Data.Drasil.Concepts.PhysicalProperties as CPP
import qualified Data.Drasil.Concepts.Physics            as CP
import           Data.Drasil.Concepts.Software
import           Data.Drasil.People
import qualified Data.Drasil.Quantities.Physics          as QP
import           Data.Drasil.SI_Units
import           Data.Drasil.Software.Products
import           Drasil.SRSDocument
import           Language.Drasil
import qualified Language.Drasil.Sentence.Combinators    as S
import           Theory.Drasil

-- This is my only 'immediate' dependency.
import           Drasil.BmBnd.SRS

caseStudy :: CI
caseStudy = commonIdeaWithDict "bmbnd" (pn "Beam Bending Analysis Program") "BmBnd" []

jasonBalaci :: Person
jasonBalaci = person "Jason" "Balaci"

purpose :: Sentence
purpose = S "To examine a beam bending under load."

si :: SystemInformation
si =
    SI
        { _sys = caseStudy
        , _kind = Doc.srs
        , _authors = [jasonBalaci]
        , _background = []
        , _purpose = [purpose]
        , _quants = [] :: [QuantityDict]
        , _concepts = [] :: [DefinedQuantityDict]
        , _instModels = [] :: [InstanceModel]
        , _datadefs = [] :: [DataDefinition]
        , _configFiles = []
        , _inputs = [] :: [QuantityDict]
        , _outputs = [] :: [QuantityDict]
        , _defSequence = [] :: [Block SimpleQDef]
        , _constraints = [] :: [ConstrainedChunk]
        , _constants = [] :: [ConstQDef]
        , _sysinfodb = symbMap
        , _usedinfodb = usedDB
        , refdb = refDB
        }

terms :: [IdeaDict]
terms =
    nw caseStudy
        : ( map nw Doc.doccon -- TODO: These items in the parentheses should be part of the 'fillCDB' background knowledge
                ++ map nw softwarecon
                ++ map nw Doc.doccon'
                ++ map nw CP.physicCon
                ++ map nw educon
                ++ [nw algorithm]
                ++ map nw derived
                ++ map nw fundamentals
                ++ map nw CM.mathcon
                ++ map nw CM.mathcon'
          )

symbMap :: ChunkDB
symbMap =
    cdb
        ([] :: [QuantityDict])
        terms
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

usedDB :: ChunkDB
usedDB =
    cdb
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
refDB = rdb [] []

realSrsBody :: SRSDecl
realSrsBody = srsBody si

srs :: Document
srs = mkDoc realSrsBody (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS realSrsBody si
