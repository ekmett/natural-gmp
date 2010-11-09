{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim,
             MagicHash, UnboxedTuples, UnliftedFFITypes #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

#include "MachDeps.h"
module GHC.Natural.GMP.Internals (
    Natural(..),

    cmpNatural#,
    cmpNaturalWord#,

    plusNatural#,
    minusNatural#,
    timesNatural#,

    quotRemNatural#,
    quotNatural#,
    remNatural#,
    divModNatural#,
    divExactNatural#,

    gcdNatural#,
    gcdNaturalWord#,
    gcdWord#,

    decodeDouble#,

    word2Natural#,
    natural2Word#,

    andNatural#,
    orNatural#,
    xorNatural#,
    complementNatural#,

    mul2ExpNatural#,
    fdivQ2ExpNatural#,

#if WORD_SIZE_IN_BITS < 64
    word64ToNatural#, naturalToWord64#,
#endif

#ifndef WORD_SIZE_IN_BITS
#error WORD_SIZE_IN_BITS not defined!!!
#endif

  ) where

import GHC.Prim
import GHC.Natural.Type

-- Double isn't available yet, and we shouldn't be using defaults anyway:
default ()

-- TODO: Could this be done in a more CPS-ey manner?

-- | Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument.
--
foreign import prim "natural_cmm_cmpNaturalzh" cmpNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> Int#

-- | Returns -1,0,1 according as first argument is less than, equal to, or greater than second argument, which
--   is an ordinary Int\#.
foreign import prim "natural_cmm_cmpNaturalWordzh" cmpNaturalWord#
  :: Word# -> ByteArray# -> Word# -> Int#

-- |
--
foreign import prim "natural_cmm_plusNaturalzh" plusNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray# #)

-- |
--
foreign import prim "natural_cmm_minusNaturalzh" minusNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray# #)

-- |
--
foreign import prim "natural_cmm_timesNaturalzh" timesNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray# #)

-- | Compute div and mod simultaneously, where div rounds towards negative
-- infinity and\ @(q,r) = divModNatural#(x,y)@ implies
-- @plusNatural# (timesNatural# q y) r = x@.
--
foreign import prim "natural_cmm_quotRemNaturalzh" quotRemNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray#, Word#, ByteArray# #)

-- | Rounds towards zero.
--
foreign import prim "natural_cmm_quotNaturalzh" quotNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray# #)

-- | Satisfies \texttt{plusNatural\# (timesNatural\# (quotNatural\# x y) y) (remNatural\# x y) == x}.
--
foreign import prim "natural_cmm_remNaturalzh" remNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray# #)

-- | Compute div and mod simultaneously, where div rounds towards negative infinity
-- and\texttt{(q,r) = divModNatural\#(x,y)} implies \texttt{plusNatural\# (timesNatural\# q y) r = x}.
--
foreign import prim "natural_cmm_divModNaturalzh" divModNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray#, Word#, ByteArray# #)

-- | Divisor is guaranteed to be a factor of dividend.
--
foreign import prim "natural_cmm_divExactNaturalzh" divExactNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray# #)

-- | Greatest common divisor.
--
foreign import prim "natural_cmm_gcdNaturalzh" gcdNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray# #)

-- | Greatest common divisor, where second argument is an ordinary {\tt Word\#}.
--
foreign import prim "natural_cmm_gcdNaturalWordzh" gcdNaturalWord#
  :: Word# -> ByteArray# -> Word# -> Word#

-- |
--
foreign import prim "natural_cmm_gcdWordzh" gcdWord#
  :: Word# -> Word# -> Word#

-- | Convert to arbitrary-precision natural.
--    First {\tt Int\#} in result is the exponent; second {\tt Word\#} and {\tt ByteArray\#}
--  represent an {\tt Natural\#} holding the mantissa.
--
foreign import prim "natural_cmm_decodeDoublezh" decodeDouble#
  :: Double# -> (# Word#, Word#, ByteArray# #)

-- |
--
foreign import prim "natural_cmm_word2Naturalzh" word2Natural#
  :: Word# -> (# Word#, ByteArray# #)

-- |
--
foreign import prim "natural_cmm_andNaturalzh" andNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray# #)

-- |
--
foreign import prim "natural_cmm_orNaturalzh" orNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray# #)

-- |
--
foreign import prim "natural_cmm_xorNaturalzh" xorNatural#
  :: Word# -> ByteArray# -> Word# -> ByteArray# -> (# Word#, ByteArray# #)

-- |
--
foreign import prim "natural_cmm_mul2ExpNaturalzh" mul2ExpNatural#
  :: Word# -> ByteArray# -> Word# -> (# Word#, ByteArray# #)

-- |
--
foreign import prim "natural_cmm_fdivQ2ExpNaturalzh" fdivQ2ExpNatural#
  :: Word# -> ByteArray# -> Word# -> (# Word#, ByteArray# #)

-- |
--
foreign import prim "natural_cmm_complementNaturalzh" complementNatural#
  :: Word# -> ByteArray# -> (# Word#, ByteArray# #)

#if WORD_SIZE_IN_BITS < 64
foreign import prim "natural_cmm_word64ToNaturalzh" word64ToNatural#
  :: Word64# -> (# Word#, ByteArray# #)

foreign import ccall unsafe "hs_naturalToWord64"
    naturalToWord64# :: Word# -> ByteArray# -> Word64#
#endif

-- used to be primops:
{-# INLINE natural2Word# #-}
natural2Word# :: ByteArray# -> Word#
natural2Word# d = indexWordArray# d 0#
