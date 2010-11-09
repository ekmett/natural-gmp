{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Natural
-- Copyright   :  (c) The University of Glasgow 1994-2008
-- License     :  see libraries/natural-gmp/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Natural' type.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
#define INT_MINBOUND (-2147483648#)
#define NEG_INT_MINBOUND (T# 2147483647# `plusNatural` T# 1#)
#elif SIZEOF_HSWORD == 8
#define INT_MINBOUND (-9223372036854775808#)
#define NEG_INT_MINBOUND (T# 9223372036854775807# `plusNatural` T# 1#)
#else
#error Unknown SIZEOF_HSWORD; can't define INT_MINBOUND and NEG_INT_MINBOUND
#endif

module GHC.Natural (
    Natural,
    smallNatural, wordToNatural, naturalToWord, toWord#,
    naturalToInteger, integerToNatural,
#if WORD_SIZE_IN_BITS < 64
    naturalToWord64, word64ToNatural,
#endif
    plusNatural, minusNatural, timesNatural,
    eqNatural, neqNatural, absNatural, signumNatural,
    leNatural, gtNatural, ltNatural, geNatural, compareNatural,
    divModNatural, quotRemNatural, quotNatural, remNatural,
    encodeFloatNatural, floatFromNatural,
    encodeDoubleNatural, decodeDoubleNatural, doubleFromNatural,
    gcdNatural, lcmNatural,
    andNatural, orNatural, xorNatural, complementNatural,
    shiftLNatural, shiftRNatural,
    hashNatural,
 ) where

import GHC.Prim (
    -- Other types we use, convert from, or convert to
    Word#, Word#, Double#, Float#, ByteArray#,
    -- Conversions between those types
    int2Word#, word2Int#, int2Double#, int2Float#,
    -- Operations on Word# that we use for operations on T#
    quotWord#, remWord#,
    (==#), (/=#), (<=#), (>=#), (<#), (>#), (*#), (-#), (+#),
    mulIntMayOflo#, addIntC#, subIntC#,
    and#, or#, xor#, not#, 
    gtWord#, geWord#, eqWord#, neWord#, ltWord#, leWord#,
    timesWord#
 )

import GHC.Natural.GMP.Internals (
    Natural(..),

    -- GMP-related primitives
    cmpNatural#, cmpNaturalWord#,
    plusNatural#, minusNatural#, timesNatural#,
    quotRemNatural#, quotNatural#, remNatural#, divModNatural#,
    gcdNatural#, gcdNaturalWord#, gcdWord#, divExactNatural#,
    decodeDouble#,
    word2Natural#, natural2Word#,
    andNatural#, orNatural#, xorNatural#, complementNatural#,
    mul2ExpNatural#, fdivQ2ExpNatural#,
#if WORD_SIZE_IN_BITS < 64
    word64ToNatural#, naturalToWord64#,
#endif
 )

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64 (
            Word64#,
            wordToWord64#, word64ToWord#,
            geWord64#, leWord64#,
       )
#endif

import GHC.Ordering
import GHC.Types
import GHC.Bool
import GHC.Integer.GMP.Internals (Integer(..))

{-# INLINE naturalToInteger #-}
naturalToInteger :: Natural -> Integer
naturalToInteger (T# i) = naturalToInteger (wordToNatural i)
naturalToInteger (X# s d) = J# (word2Int# s) d

integerToNatural :: Integer -> Natural
integerToNatural (S# i) = T# (int2Word# i)
integerToNatural (J# s d) = X# (int2Word# s) d



{-# INLINE smallNatural #-}
smallNatural :: Word# -> Natural
smallNatural i = T# i

{-# INLINE wordToNatural #-}
wordToNatural :: Word# -> Natural
wordToNatural w = case word2Natural# w of (# s, d #) -> X# s d

{-# INLINE naturalToWord #-}
naturalToWord :: Natural -> Word#
naturalToWord (T# i) = i
naturalToWord (X# _ d) = natural2Word# d

#if WORD_SIZE_IN_BITS < 64
{-# INLINE naturalToWord64 #-}
naturalToWord64 :: Natural -> Word64#
naturalToWord64 (T# i) = wordToWord64# i
naturalToWord64 (X# s d) = naturalToWord64# s d

word64ToNatural :: Word64# -> Natural
word64ToNatural w = if w `leWord64#` (wordToWord64# 0x7FFFFFFF##)
                    then T# (word64ToWord# w)
                    else case word64ToNatural# w of (# s, d #) -> X# s d

#endif

toWord# :: Natural -> Word#
{-# NOINLINE toWord# #-}
{-# RULES "toWord#" forall i. toWord# (T# i) = i #-}
-- Don't inline toWord#, because it can't do much unless
-- it sees a (T# i), and inlining just creates fruitless
-- join points.  But we do need a RULE to get the constants
-- to work right:  1::Int had better optimise to (I# 1)!
toWord# (T# i)   = i
toWord# (X# _ d) = natural2Word# d

toBig :: Natural -> Natural
toBig (T# i)     = case word2Natural# i of (# s, d #) -> X# s d
toBig i@(X# _ _) = i

-- XXX There's no good reason for us using unboxed tuples for the
-- results, but we don't have Data.Tuple available.

-- Note that we don't check for divide-by-zero here. That needs
-- to be done where it's used.
-- (we don't have error)

quotRemNatural :: Natural -> Natural -> (# Natural, Natural #)
quotRemNatural (T# i) (T# j) = (# T# q, T# r #)
    where
      -- NB. don't inline these.  (# T# (i `quotWord#` j), ... #) means
      -- (# let q = i `quotWord#` j in T# q, ... #) which builds a
      -- useless thunk.  Placing the bindings here means they'll be
      -- evaluated strictly.
      !q = i `quotWord#` j
      !r = i `remWord#`  j
quotRemNatural i1@(X# _ _) i2@(T# _) = quotRemNatural i1 (toBig i2)
quotRemNatural i1@(T# _) i2@(X# _ _) = quotRemNatural (toBig i1) i2
quotRemNatural (X# s1 d1) (X# s2 d2)
  = case (quotRemNatural# s1 d1 s2 d2) of
          (# s3, d3, s4, d4 #)
            -> (# X# s3 d3, X# s4 d4 #)

divModNatural :: Natural -> Natural -> (# Natural, Natural #)
divModNatural = quotRemNatural

remNatural :: Natural -> Natural -> Natural
remNatural (T# a) (T# b) = T# (remWord# a b)
{- Special case doesn't work, because a 1-element X# _ has the range
   -(2^32-1) -- 2^32-1, whereas T# has the range -2^31 -- (2^31-1)
remNatural ia@(T# a) (X# sb b)
  | sb `eqWord#` 1#  = T# (remWord# a (word2Word# (natural2Word# sb b)))
  | sb `eqWord#` -1# = T# (remWord# a (0## -# (word2Word# (natural2Word# sb b))))
  | 0## <# sb   = ia
  | otherwise  = T# (0## -# a)
-}
remNatural ia@(T# _) ib@(X# _ _) = remNatural (toBig ia) ib
remNatural (X# s1 d1) (T# b) = case word2Natural# b of 
                               (# s2, d2 #) -> case remNatural# s1 d2 s2 d2 of
                                 (# _, d3 #) -> T# (natural2Word# d3)
remNatural (X# s1 d1) (X# s2 d2) = case remNatural# s1 d1 s2 d2 of (# s3, d3 #) -> X# s3 d3

quotNatural :: Natural -> Natural -> Natural
quotNatural (T# a) (T# b) = T# (quotWord# a b)
{- Special case disabled, see remNatural above
quotNatural (T# a) (X# _ sb b)
  | sb `eqWord#` 1#  = T# (quotWord# a (word2Word# (natural2Word# sb b)))
  | sb `eqWord#` -1# = T# (quotWord# a (0## -# (word2Word# (natural2Word# sb b))))
  | otherwise  = T# 0
-}
quotNatural ia@(T# _) ib@(X# _ _) = quotNatural (toBig ia) ib
quotNatural (X# s1 d1) (T# b) = case word2Natural# b of 
                                  (# s2, d2 #) ->
                                    case quotNatural# s1 d1 s2 d2 of
                                      (# s3, d3 #) -> X# s3 d3
quotNatural (X# s1 d1) (X# s2 d2) = case quotNatural# s1 d1 s2 d2 of (# s3, d3 #) -> X# s3 d3



-- We can't throw an error here, so it is up to our caller to
-- not call us with both arguments being 0.
gcdNatural :: Natural -> Natural -> Natural
-- SUP: Do we really need the first two cases?
gcdNatural (T# a) (T# b) = T# (gcdWord a b)
gcdNatural (T# 0##) ib = ib
gcdNatural (T# a) (X# s d) = T# (gcdNaturalWord# s d a)
gcdNatural ia@(X# _ _) ib@(T# _) = gcdNatural ib ia
gcdNatural (X# s1 d1) (X# s2 d2) = case gcdNatural# s1 d1 s2 d2 of (# s3, d3 #) -> X# s3 d3

lcmNatural :: Natural -> Natural -> Natural
lcmNatural a b =      if a `eqNatural` T# 0## then T# 0##
                 else if b `eqNatural` T# 0## then T# 0##
                 else (divExact a (gcdNatural a b)) `timesNatural` b

{-# RULES "gcdNatural/Int" forall a b.
            gcdNatural (T# a) (T# b) = T# (gcdWord a b)
  #-}
gcdWord :: Word# -> Word# -> Word#
gcdWord 0## y  = y
gcdWord x  0## = x
gcdWord x  y  = gcdWord# x y

divExact :: Natural -> Natural -> Natural
divExact (T# a) (T# b) = T# (quotWord# a b)
divExact (T# a) (X# _ b) = T# (quotWord# a (natural2Word# b))
divExact (X# s1 d1) (T# b) = case word2Natural# b of 
                                  (# s2, d2 #) ->
                                    case divExactNatural# s1 d1 s2 d2 of
                                      (# s3, d3 #) -> X# s3 d3
divExact (X# s1 d1) (X# s2 d2) = case divExactNatural# s1 d1 s2 d2 of (# s3, d3 #) -> X# s3 d3


eqNatural :: Natural -> Natural -> Bool
eqNatural (T# i)     (T# j)     = i `eqWord#` j
eqNatural (T# i)     (X# s d)   = cmpNaturalWord# s d i   ==# 0#
eqNatural (X# s d)   (T# i)     = cmpNaturalWord# s d i   ==# 0#
eqNatural (X# s1 d1) (X# s2 d2) = cmpNatural# s1 d1 s2 d2 ==# 0#

neqNatural :: Natural -> Natural -> Bool
neqNatural (T# i)     (T# j)     = i `neWord#` j
neqNatural (T# i)     (X# s d)   = cmpNaturalWord# s d i   /=# 0#
neqNatural (X# s d)   (T# i)     = cmpNaturalWord# s d i   /=# 0#
neqNatural (X# s1 d1) (X# s2 d2) = cmpNatural# s1 d1 s2 d2 /=# 0#

------------------------------------------------------------------------

leNatural :: Natural -> Natural -> Bool
leNatural (T# i)     (T# j)     = i `leWord#` j
leNatural (X# s d)   (T# i)     = cmpNaturalWord# s d i   <=# 0#
leNatural (T# i)     (X# s d)   = cmpNaturalWord# s d i   >=# 0#
leNatural (X# s1 d1) (X# s2 d2) = cmpNatural# s1 d1 s2 d2 <=# 0#

gtNatural :: Natural -> Natural -> Bool
gtNatural (T# i)     (T# j)     = i `gtWord#` j
gtNatural (X# s d)   (T# i)     = cmpNaturalWord# s d i   ># 0#
gtNatural (T# i)     (X# s d)   = cmpNaturalWord# s d i   <# 0#
gtNatural (X# s1 d1) (X# s2 d2) = cmpNatural# s1 d1 s2 d2 ># 0#

ltNatural :: Natural -> Natural -> Bool
ltNatural (T# i)     (T# j)     = i `ltWord#` j
ltNatural (X# s d)   (T# i)     = cmpNaturalWord# s d i   <# 0#
ltNatural (T# i)     (X# s d)   = cmpNaturalWord# s d i   ># 0#
ltNatural (X# s1 d1) (X# s2 d2) = cmpNatural# s1 d1 s2 d2 <# 0#

geNatural :: Natural -> Natural -> Bool
geNatural (T# i)     (T# j)     = i `geWord#` j
geNatural (X# s d)   (T# i)     = cmpNaturalWord# s d i   >=# 0#
geNatural (T# i)     (X# s d)   = cmpNaturalWord# s d i   <=# 0#
geNatural (X# s1 d1) (X# s2 d2) = cmpNatural# s1 d1 s2 d2 >=# 0#

compareNatural :: Natural -> Natural -> Ordering
compareNatural (T# i)  (T# j)
   =      if i `eqWord#` j then EQ
     else if i `ltWord#` j then LT
     else                 GT
compareNatural (X# s d) (T# i)
   = case cmpNaturalWord# s d i of { res# ->
     if res# <# 0# then LT else
     if res# ># 0# then GT else EQ
     }
compareNatural (T# i) (X# s d)
   = case cmpNaturalWord# s d i of { res# ->
     if res# ># 0# then LT else
     if res# <# 0# then GT else EQ
     }
compareNatural (X# s1 d1) (X# s2 d2)
   = case cmpNatural# s1 d1 s2 d2 of { res# ->
     if res# <# 0# then LT else
     if res# ># 0# then GT else EQ
     }

-- id!
absNatural :: Natural -> Natural
absNatural x = x

signumNatural :: Natural -> Natural
signumNatural (T# 0##) = T# 0##
signumNatural (T# _)   = T# 1##
signumNatural (X# 0## _) = T# 0##
signumNatural (X# s d)
  = let
        !cmp = cmpNaturalWord# s d 0##
    in
    if cmp ># 0# then T# 1##
    else T# 0##

-- This really wants addWordC#
plusNatural :: Natural -> Natural -> Natural
plusNatural i1@(T# i) i2@(T# j)  = case addIntC# (word2Int# i) (word2Int# j) of
                                   (# r, 0# #) -> T# (int2Word# r)
                                   (# _, _  #) -> plusNatural (toBig i1) (toBig i2)
plusNatural i1@(X# _ _) i2@(T# _) = plusNatural i1 (toBig i2)
plusNatural i1@(T# _) i2@(X# _ _) = plusNatural (toBig i1) i2
plusNatural (X# s1 d1) (X# s2 d2) = case plusNatural# s1 d1 s2 d2 of (# s3, d3 #) -> X# s3 d3

minusNatural :: Natural -> Natural -> Natural
minusNatural i1@(T# i) i2@(T# j)   = case subIntC# (word2Int# i) (word2Int# j) of
                                     (# r, 0# #) -> T# (int2Word# r)
                                     (# _, _  #) -> minusNatural (toBig i1) (toBig i2)
minusNatural i1@(X# _ _) i2@(T# _) = minusNatural i1 (toBig i2)
minusNatural i1@(T# _) i2@(X# _ _) = minusNatural (toBig i1) i2
minusNatural (X# s1 d1) (X# s2 d2) = case minusNatural# s1 d1 s2 d2 of (# s3, d3 #) -> X# s3 d3

timesNatural :: Natural -> Natural -> Natural
timesNatural i1@(T# i) i2@(T# j)   = if mulIntMayOflo# (word2Int# i) (word2Int# j) ==# 0#
                                     then T# (i `timesWord#` j)
                                     else timesNatural (toBig i1) (toBig i2)
timesNatural i1@(X# _ _) i2@(T# _) = timesNatural i1 (toBig i2)
timesNatural i1@(T# _) i2@(X# _ _) = timesNatural (toBig i1) i2
timesNatural (X# s1 d1) (X# s2 d2) = case timesNatural# s1 d1 s2 d2 of (# s3, d3 #) -> X# s3 d3

encodeFloatNatural :: Natural -> Word# -> Float#
encodeFloatNatural (T# i) j = word_encodeFloat# i j
encodeFloatNatural (X# _ d) e = encodeFloat# d e

encodeDoubleNatural :: Natural -> Word# -> Double#
encodeDoubleNatural (T# i) j = word_encodeDouble# i j
encodeDoubleNatural (X# _ d) e = encodeDouble# d e

decodeDoubleNatural :: Double# -> (# Natural, Word# #)
decodeDoubleNatural d = case decodeDouble# d of
                        (# exp, s, d #) -> (# X# s d, exp #)

-- previous code: doubleFromNatural n = fromNatural n = encodeFloat n 0
-- doesn't work too well, because encodeFloat is defined in
-- terms of ccalls which can never be simplified away.  We
-- want simple literals like (fromNatural 3 :: Float) to turn
-- into (F# 3.0), hence the special case for T# here.

doubleFromNatural :: Natural -> Double#
doubleFromNatural (T# i) = int2Double# (word2Int# i)
doubleFromNatural (X# _ d) = encodeDouble# d 0##

floatFromNatural :: Natural -> Float#
floatFromNatural (T# i) = int2Float# (word2Int# i)
floatFromNatural (X# _ d) = encodeFloat# d 0##

foreign import ccall unsafe "natural_cbits_encodeFloat"
        encodeFloat# :: ByteArray# -> Word# -> Float#
foreign import ccall unsafe "__word_encodeFloat"
        word_encodeFloat# :: Word# -> Word# -> Float#

foreign import ccall unsafe "natural_cbits_encodeDouble"
        encodeDouble# :: ByteArray# -> Word# -> Double#
foreign import ccall unsafe "__word_encodeDouble"
        word_encodeDouble# :: Word# -> Word# -> Double#


andNatural :: Natural -> Natural -> Natural
(T# x) `andNatural` (T# y) = T# (x `and#` y)
x@(T# _) `andNatural` y = toBig x `andNatural` y
x `andNatural` y@(T# _) = x `andNatural` toBig y
(X# s1 d1) `andNatural` (X# s2 d2) = case andNatural# s1 d1 s2 d2 of (# s3, d3 #) -> X# s3 d3

orNatural :: Natural -> Natural -> Natural
(T# x) `orNatural` (T# y) = T# (x `or#` y)
x@(T# _) `orNatural` y = toBig x `orNatural` y
x `orNatural` y@(T# _) = x `orNatural` toBig y
(X# s1 d1) `orNatural` (X# s2 d2) = case orNatural# s1 d1 s2 d2 of (# s3, d3 #) -> X# s3 d3

xorNatural :: Natural -> Natural -> Natural
(T# x) `xorNatural` (T# y) = T# (x `xor#` y)
x@(T# _) `xorNatural` y = toBig x `xorNatural` y
x `xorNatural` y@(T# _) = x `xorNatural` toBig y
(X# s1 d1) `xorNatural` (X# s2 d2) = case xorNatural# s1 d1 s2 d2 of (# s3, d3 #) -> X# s3 d3


complementNatural :: Natural -> Natural
complementNatural (T# x) = T# (not# x)
complementNatural (X# s1 d1) = case complementNatural# s1 d1 of (# s2, d2 #) -> X# s2 d2

shiftLNatural :: Natural -> Word# -> Natural
shiftLNatural j@(T# _) i = shiftLNatural (toBig j) i
shiftLNatural (X# s1 d1) i = case mul2ExpNatural# s1 d1 i of (# s2, d2 #) -> X# s2 d2

shiftRNatural :: Natural -> Word# -> Natural
shiftRNatural j@(T# _) i = shiftRNatural (toBig j) i
shiftRNatural (X# s1 d1) i = case fdivQ2ExpNatural# s1 d1 i of (# s2, d2 #) -> X# s2 d2


-- This is used by hashUnique

-- | hashNatural returns the same value as 'fromIntegral', although in
-- unboxed form.  It might be a reasonable hash function for 'Natural', 
-- given a suitable distribution of 'Natural' values.

hashNatural :: Natural -> Word#
hashNatural = toWord#
                              
