{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module GHC.Natural.Type (Natural(..)) where

import GHC.Prim

-- | Arbitrary-precision naturals.
data Natural
   = T# Word#                            -- small naturals
   | X# Word# ByteArray#                 -- large naturals (the first Word# is the number of used limbs in the ByteArray#)

