module Drasil.BmBnd.References where

import           Language.Drasil (Reference, BibRef)
import           Data.Drasil.Citations

-- FIXME: All of these are 'background' knowledge, they should be a part of the
-- fillCdb imports -- presupposed! Additionally, if we add to this list, they
-- will appear on the list of references even though they are not _actually_
-- used.
references :: BibRef
references = [ parnasClements1986
             , koothoor2013
             , smithLai2005]
