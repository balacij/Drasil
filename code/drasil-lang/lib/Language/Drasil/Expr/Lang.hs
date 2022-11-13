{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

-- | The Drasil Expression language 
module Language.Drasil.Expr.Lang where

import Language.Drasil.Literal.Lang (Literal(..))
import Language.Drasil.Space (DiscreteDomainDesc, RealInterval, Space)
import qualified Language.Drasil.Space as S
import Language.Drasil.UID (UID)
import Language.Drasil.Literal.Class (LiteralC(..))
import Language.Drasil.WellTyped

-- * Expression Types

-- | A relation is just an expression ('Expr').
type Relation = Expr

-- | The variable type is just a renamed 'String'.
type Variable = String

-- Binary functions

-- | Arithmetic operators (fractional, power, and subtraction).
data ArithBinOp = Frac | Pow | Subt
  deriving Eq

-- | Equality operators (equal or not equal).
data EqBinOp = Eq | NEq
  deriving Eq

-- | Conditional and Biconditional operators (Expressions can imply
-- one another, or exist if and only if another expression exists).
data BoolBinOp = Impl | Iff
  deriving Eq

-- | Index operator.
data LABinOp = Index
  deriving Eq

-- | Ordered binary operators (less than, greater than, less than or equal to, greater than or equal to).
data OrdBinOp = Lt | Gt | LEq | GEq
  deriving Eq

-- | @Vector x Vector -> Vector@ binary operations (cross product).
data VVVBinOp = Cross
  deriving Eq

-- | @Vector x Vector -> Number@ binary operations (dot product).
data VVNBinOp = Dot
  deriving Eq

-- | Associative operators (adding and multiplication). Also specifies whether it is for integers or for real numbers.
data AssocArithOper = AddI | AddRe | MulI | MulRe
  deriving Eq

-- | Associative boolean operators (and, or).
data AssocBoolOper = And | Or
  deriving Eq

-- | Unary functions (abs, log, ln, sin, etc.).
data UFunc = Abs | Log | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin
  | Arccos | Arctan | Exp | Sqrt | Neg
  deriving Eq

-- | @Bool -> Bool@ operators.
data UFuncB = Not
  deriving Eq

-- | @Vector -> Vector@ operators.
data UFuncVV = NegV
  deriving Eq

-- | @Vector -> Number@ operators.
data UFuncVN = Norm | Dim
  deriving Eq

-- | For case expressions (either complete or incomplete).
data Completeness = Complete | Incomplete
  deriving Eq

-- ** Expr

-- | Expression language where all terms are supposed to be 'well understood'
--   (i.e., have a definite meaning). Right now, this coincides with
--   "having a definite value", but should not be restricted to that.
data Expr where
  -- | Brings a literal into the expression language.
  Lit :: Literal -> Expr
  -- | Takes an associative arithmetic operator with a list of expressions.
  AssocA   :: AssocArithOper -> [Expr] -> Expr
  -- | Takes an associative boolean operator with a list of expressions.
  AssocB   :: AssocBoolOper  -> [Expr] -> Expr
  -- | C stands for "Chunk", for referring to a chunk in an expression.
  --   Implicitly assumes that the chunk has a symbol.
  C        :: UID -> Expr
  -- | A function call accepts a list of parameters and a list of named parameters.
  --   For example
  --
  --   * F(x) is (FCall F [x] []).
  --   * F(x,y) would be (FCall F [x,y]).
  --   * F(x,n=y) would be (FCall F [x] [(n,y)]).
  FCall    :: UID -> [Expr] -> [(UID, Expr)] -> Expr
  -- | For multi-case expressions, each pair represents one case.
  Case     :: Completeness -> [(Expr, Relation)] -> Expr
  -- | Represents a matrix of expressions.
  Matrix   :: [[Expr]] -> Expr

  -- | Unary operation for most functions (eg. sin, cos, log, etc.).
  UnaryOp       :: UFunc -> Expr -> Expr
  -- | Unary operation for @Bool -> Bool@ operations.
  UnaryOpB      :: UFuncB -> Expr -> Expr
  -- | Unary operation for @Vector -> Vector@ operations.
  UnaryOpVV     :: UFuncVV -> Expr -> Expr
  -- | Unary operation for @Vector -> Number@ operations.
  UnaryOpVN     :: UFuncVN -> Expr -> Expr

  -- | Binary operator for arithmetic between expressions (fractional, power, and subtraction).
  ArithBinaryOp :: ArithBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for boolean operators (implies, iff).
  BoolBinaryOp  :: BoolBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for equality between expressions.
  EqBinaryOp    :: EqBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for indexing two expressions.
  LABinaryOp    :: LABinOp -> Expr -> Expr -> Expr
  -- | Binary operator for ordering expressions (less than, greater than, etc.).
  OrdBinaryOp   :: OrdBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for @Vector x Vector -> Vector@ operations (cross product).
  VVVBinaryOp   :: VVVBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for @Vector x Vector -> Number@ operations (dot product).
  VVNBinaryOp   :: VVNBinOp -> Expr -> Expr -> Expr

  -- | Operators are generalized arithmetic operators over a 'DomainDesc'
  --   of an 'Expr'.  Could be called BigOp.
  --   ex: Summation is represented via 'Add' over a discrete domain.
  Operator :: AssocArithOper -> DiscreteDomainDesc Expr Expr -> Expr -> Expr
  -- | A different kind of 'IsIn'. A 'UID' is an element of an interval.
  RealI    :: UID -> RealInterval Expr Expr -> Expr

-- | Expressions are equal if their constructors and contents are equal.
instance Eq Expr where
  Lit (Int l)         == Lit (Int r)         =  l == r
  Lit (Str l)         == Lit (Str r)         =  l == r
  Lit (Dbl l)         == Lit (Dbl r)         =  l == r
  Lit (ExactDbl l)    == Lit (ExactDbl r)    =  l == r
  Lit (Perc l1 l2)    == Lit (Perc r1 r2)    =  l1 == r1 && l2 == r2
  -- Lit a               == Lit b               =   a == b -- TODO: When we have typed expressions, I think this will be possible.
  AssocA o1 l1        == AssocA o2 l2        =  o1 == o2 && l1 == l2
  AssocB o1 l1        == AssocB o2 l2        =  o1 == o2 && l1 == l2
  C a                 == C b                 =   a == b
  FCall a b c         == FCall d e f         =   a == d && b == e && c == f
  Case a b            == Case c d            =   a == c && b == d
  UnaryOp a b         == UnaryOp c d         =   a == c && b == d
  UnaryOpB a b        == UnaryOpB c d        =   a == c && b == d
  UnaryOpVV a b       == UnaryOpVV c d       =   a == c && b == d
  UnaryOpVN a b       == UnaryOpVN c d       =   a == c && b == d
  ArithBinaryOp o a b == ArithBinaryOp p c d =   o == p && a == c && b == d
  BoolBinaryOp o a b  == BoolBinaryOp p c d  =   o == p && a == c && b == d
  EqBinaryOp o a b    == EqBinaryOp p c d    =   o == p && a == c && b == d
  OrdBinaryOp o a b   == OrdBinaryOp p c d   =   o == p && a == c && b == d
  LABinaryOp o a b    == LABinaryOp p c d    =   o == p && a == c && b == d
  VVVBinaryOp o a b   == VVVBinaryOp p c d   =   o == p && a == c && b == d
  VVNBinaryOp o a b   == VVNBinaryOp p c d   =   o == p && a == c && b == d
  _                   == _                   =   False
-- ^ TODO: This needs to add more equality checks

-- instance Num Expr where
--   (Int 0)        + b              = b
--   a              + (Int 0)        = a
--   (AssocA Add l) + (AssocA Add m) = AssocA Add (l ++ m)
--   (AssocA Add l) + b              = AssocA Add (l ++ [b])
--   a              + (AssocA Add l) = AssocA Add (a : l)
--   a              + b              = AssocA Add [a, b]

--   (AssocA Mul l) * (AssocA Mul m) = AssocA Mul (l ++ m)
--   (AssocA Mul l) * b              = AssocA Mul (l ++ [b])
--   a              * (AssocA Mul l) = AssocA Mul (a : l)
--   a              * b              = AssocA Mul [a, b]

--   a - b = ArithBinaryOp Subt a b

--   fromInteger = Int
--   abs         = UnaryOp Abs
--   negate      = UnaryOp Neg

--   -- this is a Num wart
--   signum _ = error "should not use signum in expressions"

-- instance Fractional Expr where
--   a / b = ArithBinaryOp Frac a b
--   fromRational r = ArithBinaryOp Frac (fromInteger $ numerator   r)
--                                       (fromInteger $ denominator r)

instance LiteralC Expr where
  int = Lit . int
  str = Lit . str
  dbl = Lit . dbl
  exactDbl = Lit . exactDbl
  perc l r = Lit $ perc l r

expectedAssocATy :: AssocArithOper -> Space
expectedAssocATy AddI = S.Integer
expectedAssocATy MulI = S.Integer
expectedAssocATy _ = S.Real

expUnaryOpTy :: UFunc -> (Space, Space)
expUnaryOpTy Abs = _wa
expUnaryOpTy Log = _wb
expUnaryOpTy Ln = _wc
expUnaryOpTy Sin = _wd
expUnaryOpTy Cos = _we
expUnaryOpTy Tan = _wf
expUnaryOpTy Sec = _wg
expUnaryOpTy Csc = _wh
expUnaryOpTy Cot = _wi
expUnaryOpTy Arcsin = _wj
expUnaryOpTy Arccos = _wk
expUnaryOpTy Arctan = _wl
expUnaryOpTy Exp = _wm
expUnaryOpTy Sqrt = _wn
expUnaryOpTy Neg = _wo

instance Typed Expr Space where
  infer :: TypingContext Space -> Expr -> Either Space TypeError
  infer cxt (Lit lit) = infer cxt lit
  
  infer cxt (AssocA op exs)
    | allOfType cxt exs t = Left t
    | otherwise = Right "Associative arithmetic operation does not contain strictly the same numeric type."
      where t = expectedAssocATy op

  infer cxt (AssocB _ exs)
    | allOfType cxt exs S.Boolean = Left S.Boolean
    | otherwise = Right "Associative boolean operation does not contain strictly boolean operands."
  
  infer cxt (C uid) = inferFromContext cxt uid

  infer cxt (FCall uid exs x0) = _we
  
  infer cxt (Case _ ers) -- = _ -- all (\(e, r) -> infer cxt e) ers
    | null ers = Right "Case contains no expressions, no type to infer."
    | all (\(ne, _) -> infer cxt ne == eT) (tail ers) = eT
    | otherwise = Right "Expressions in case statement contain different types."
      where
        (fe, _) = head ers
        eT = infer cxt fe
  
  infer cxt (Matrix exss)
    | null exss = Right "Matrix has no rows."
    | null $ head exss = Right "Matrix has no columns."
    | allRowsHaveSameColumnsAndSpace = Left $ S.Matrix rows columns t
    | otherwise = Right "Not all rows have the same number of columns or the same value types."
    where
        rows = length exss
        columns = if rows > 0 then length $ head exss else 0
        sss = map (map (infer cxt)) exss
        expT = head $ head sss
        allRowsHaveSameColumnsAndSpace
          = either
              (\_ -> all (\ r -> length r == columns && all (== expT) r) sss)
              (const False) expT
        (Left t) = expT
  
  infer cxt (UnaryOp uf ex) = case infer cxt ex of
    Left sp -> _
    x -> x
  
  infer cxt (UnaryOpB ufb ex) = _wi
  infer cxt (UnaryOpVV ufv ex) = _wj
  infer cxt (UnaryOpVN ufv ex) = _wk
  infer cxt (ArithBinaryOp abo ex ex') = _wl
  infer cxt (BoolBinaryOp bbo ex ex') = _wm
  infer cxt (EqBinaryOp ebo ex ex') = _wn
  infer cxt (LABinaryOp lbo ex ex') = _wo
  infer cxt (OrdBinaryOp obo ex ex') = _wp
  infer cxt (VVVBinaryOp vbo ex ex') = _wq
  infer cxt (VVNBinaryOp vbo ex ex') = _wr
  infer cxt (Operator aao dd ex) = _ws

  infer cxt (RealI uid ri) = _wt
