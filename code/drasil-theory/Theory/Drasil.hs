{- re-export many things to simplify external use -}
module Theory.Drasil (
  -- Classes
    HasInputs(..), HasOutput(..)
  -- DataDefinition
  , DataDefinition, mkQuantDef, mkQuantDef', dd, ddNoRefs, qdFromDD
  -- GenDefn
  , GenDefn, gd, gdNoRefs, gdMK, gdMKNoRefs, gdGetEqMods
  -- InstanceModel
  , InstanceModel
  , im, imNoDeriv, imNoRefs, imNoDerivNoRefs, imGetEqMods
  , qwUC, qwC
  -- ModelKinds
  , ModelKinds(..), getEqMods
  -- Theory
  , Theory(..), TheoryModel, tm, tmNoRefs
) where

import Theory.Drasil.Classes (HasInputs(..), HasOutput(..))
import Theory.Drasil.DataDefinition (DataDefinition, mkQuantDef, mkQuantDef',
  dd, ddNoRefs, qdFromDD)
import Theory.Drasil.GenDefn
    ( gd, gdNoRefs, GenDefn, gdMK, gdMKNoRefs, gdGetEqMods )
import Theory.Drasil.InstanceModel
    ( InstanceModel,
      im,
      imNoDeriv,
      imNoRefs,
      imNoDerivNoRefs,
      imGetEqMods,
      qwUC,
      qwC )
import Theory.Drasil.ModelKinds (ModelKinds(..), getEqMods)
import Theory.Drasil.Theory
