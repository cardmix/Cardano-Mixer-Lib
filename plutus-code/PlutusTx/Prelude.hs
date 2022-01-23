-- Need some extra imports from the Prelude for doctests, annoyingly
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fmax-simplifier-iterations=0 #-}

module PlutusTx.Prelude (
    -- $prelude
    -- * Classes
    module Eq,
    module Enum,
    module Ord,
    module Semigroup,
    module Monoid,
    module Numeric,
    module Functor,
    module Applicative,
    module Lattice,
    module Foldable,
    module Traversable,
    -- * Monad
    (Haskell.>>=),
    (Haskell.=<<),
    (Haskell.>>),
    Haskell.return,
    -- * Standard functions, Tuples
    module Base,
    -- * Error
    error,
    check,
    -- * Booleans
    module Bool,
    -- * Integer numbers
    Integer,
    divide,
    modulo,
    quotient,
    remainder,
    even,
    -- * Maybe
    module Maybe,
    -- * Either
    module Either,
    -- * Lists
    module List,
    -- * Rational numbers
    Rational,
    unsafeRatio,
    ratio,
    fromInteger,
    round
    ) where

import Data.String (IsString (..))
import PlutusTx.Applicative as Applicative
import PlutusTx.Base as Base
import PlutusTx.Bool as Bool
import PlutusTx.Builtins (Integer, error)
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Either as Either
import PlutusTx.Enum as Enum
import PlutusTx.Eq as Eq
import PlutusTx.Foldable as Foldable
import PlutusTx.Functor as Functor
import PlutusTx.Lattice as Lattice
import PlutusTx.List as List hiding (foldr)
import PlutusTx.Maybe as Maybe
import PlutusTx.Monoid as Monoid
import PlutusTx.Numeric as Numeric
import PlutusTx.Ord as Ord
import PlutusTx.Ratio as Ratio
import PlutusTx.Semigroup as Semigroup
import PlutusTx.Traversable as Traversable

import qualified Prelude as Haskell (return, (=<<), (>>), (>>=), error)

-- this module does lots of weird stuff deliberately
{- HLINT ignore -}

-- $prelude
-- The PlutusTx Prelude is a replacement for the Haskell Prelude that works
-- better with Plutus Tx. You should use it if you're writing code that
-- will be compiled with the Plutus Tx compiler.
-- @
--     {-# LANGUAGE NoImplicitPrelude #-}
--     import PlutusTx.Prelude
-- @

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> import PlutusTx.Prelude

{-# INLINABLE check #-}
-- | Checks a 'Bool' and aborts if it is false.
check :: Bool -> ()
check b = if b then () else Haskell.error "checkHasFailedError"

{-# INLINABLE divide #-}
-- | Integer division, rounding downwards
--
--   >>> divide (-41) 5
--   -9
--
divide :: Integer -> Integer -> Integer
divide = Builtins.divideInteger

{-# INLINABLE modulo #-}
-- | Integer remainder, always positive for a positive divisor
--
--   >>> modulo (-41) 5
--   4
--
modulo :: Integer -> Integer -> Integer
modulo = Builtins.modInteger

{-# INLINABLE quotient #-}
-- | Integer division, rouding towards zero
--
--   >>> quotient (-41) 5
--   -8
--

quotient :: Integer -> Integer -> Integer
quotient = Builtins.quotientInteger

{-# INLINABLE remainder #-}
-- | Integer remainder, same sign as dividend
--
--   >>> remainder (-41) 5
--   -1
--
remainder :: Integer -> Integer -> Integer
remainder = Builtins.remainderInteger

{-# INLINABLE even #-}
even :: Integer -> Bool
even n = if modulo n 2 == 0 then True else False

