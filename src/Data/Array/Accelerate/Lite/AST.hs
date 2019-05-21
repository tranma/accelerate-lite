{-# LANGUAGE GADTs #-}
-- |
-- Following Accelerate, we stratify the embedded array language into two languages,
-- one on scalar expressions, and one on collection operations.
--
module Data.Array.Accelerate.Lite.AST where

data Acc a where
  ALet :: Acc a
       -> Acc a
       -> Acc a

  AVar :: DeBruijn
       -> Acc a

  ATuple :: (Acc a, Acc b)
         -> Acc (a, b)

  AProj :: TupleIndex
        -> Acc a
        -> Acc b

  AApp :: Fun (a -> b)
       -> Acc a
       -> Acc b

  ACond :: Exp Bool
        -> Acc a
        -> Acc a
        -> Acc a

  AUse :: a
       -> Acc a

  AUnit :: Exp e
        -> Acc (Array DIM0 e)

  AMap :: Fun (e -> e')
       -> Acc (Array sh e)
       -> Acc (Array sh e)

data Exp a where
  XLet :: Exp a
       -> Exp b
       -> Exp b

  XVar :: DeBruijn
       -> Exp a

  XTuple :: (Exp a, Exp b)
         -> Exp (a, b)

  XProject :: TupleIndex
           -> Exp a
           -> Exp b

  XCond :: Exp Bool
        -> Exp a
        -> Exp a
        -> Exp a

  XConst :: a
         -> Exp a

  XPrimApp :: PrimFun (a -> b)
           -> Exp a
           -> Exp b

data Fun t where
  Body :: Exp t -> Fun t
  Lam :: Fun t -> Fun t

data Array sh e where
  Array :: sh -> ArrayData e -> Array sh e

data Z = Z

type DIM0 = Z

newtype ArrayData a = ArrayData [a]

newtype DeBruijn = DeBruijn Int

newtype TupleIndex = TupleIndex Int

data PrimFun sig where
  PrimAdd :: PrimFun ((a, a) -> a)
