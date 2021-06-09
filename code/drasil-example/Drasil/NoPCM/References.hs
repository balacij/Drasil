module Drasil.NoPCM.References (citations, citeRefs) where

import Language.Drasil

import Drasil.SWHS.References (incroperaEtAl2007, koothoor2013, lightstone2012, 
  parnasClements1986, smithLai2005)

citations :: BibRef
citations = [incroperaEtAl2007, koothoor2013, lightstone2012, parnasClements1986, smithLai2005]

citeRefs :: [Reference]
citeRefs = map rw citations
