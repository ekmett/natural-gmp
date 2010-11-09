{-# LANGUAGE MagicHash, Rank2Types #-}
module GHC.Natural.Lazy where

import GHC.Word
import GHC.Prim
import GHC.Integer
import GHC.Integer.GMP.Internals
import qualified GHC.Natural as N
import qualified GHC.Natural.GMP.Internals as NI

class Monoid m where
  unit :: m
  (<>) :: m -> m -> m

data Sum = Sum Word# ByteArray#

instance Monoid b => Monoid (a -> b) where
  unit _ = unit
  (f <> g) x = f x <> g x

instance Monoid Sum where
  unit = unit
  (<>) = (<>)

-- Does this buy us anything over a regular difference natural?
newtype Natural = Natural { unNatural :: forall m. Monoid m => (Word# -> ByteArray# -> m) -> m }

instance Monoid Natural where
  unit = Natural unit
  Natural f <> Natural g = Natural (f <> g)

  
naturalToInteger :: Natural -> Integer
naturalToInteger (Natural f) = case f Sum of Sum s d -> J# (word2Int# s) d

