{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.ModelExpr.Class where

import Prelude hiding (sqrt, log, sin, cos, tan, exp)

import Control.Lens ((^.))

import Language.Drasil.ModelExpr.Lang (ModelExpr(..), DerivType(..),
  SpaceBinOp(..), StatBinOp(..), AssocBoolOper(..))
import Language.Drasil.Space (Space)
import Language.Drasil.Classes.Core (HasSymbol, HasUID(..))

  
-- | Helper for creating new smart constructors for Associative Binary
--   operations that require at least 1 expression.
assocCreate :: AssocBoolOper -> [ModelExpr] -> ModelExpr
assocCreate abo [] = error $ "Need at least 1 expression to create " ++ show abo
assocCreate _ [x]  = x
assocCreate b des  = AssocB b $ assocSanitize b des
  
-- | Helper for associative operations, removes embedded variants of the same kind
assocSanitize :: AssocBoolOper -> [ModelExpr] -> [ModelExpr]
assocSanitize _ [] = []
assocSanitize b (it@(AssocB c des):r)
  | b == c    = assocSanitize b des ++ assocSanitize b r
  | otherwise = it : assocSanitize b r
assocSanitize b (de:des) = de : assocSanitize b des

class ModelExprC r where
  -- This also wants a symbol constraint.
  -- | Gets the derivative of an 'ModelExpr' with respect to a 'Symbol'.
  deriv, pderiv :: (HasUID c, HasSymbol c) => r -> c -> r
  
  -- TODO: This "defines" is odd, it should really be accepting a (HasUID c, HasSymbol c) => c as a first parameter, I think?
  -- | One expression is "defined" by another.
  defines :: r -> r -> r
  
  space :: Space -> r

  isIn :: r -> Space -> r
  
  -- | Binary associative "And".
  andMEs :: [r] -> r
  
  -- | Binary associative "Equivalence".
  equivMEs :: [r] -> r

instance ModelExprC ModelExpr where
  deriv e c  = Deriv Total e (c ^. uid)
  pderiv e c = Deriv Part  e (c ^. uid)
  
  defines = StatBinaryOp Defines

  space = Spc

  isIn a s = SpaceBinaryOp IsIn a (Spc s)

  andMEs = assocCreate And
  
  equivMEs des
    | length des >= 2 = assocCreate Equivalence des
    | otherwise       = error $ "Need at least 2 expressions to create " ++ show Equivalence
 
