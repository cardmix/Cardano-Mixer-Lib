{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
-- This ensures that we don't put *anything* about these functions into the interface
-- file, otherwise GHC can be clever about the ones that are always error, even though
-- they're NOINLINE!
{-# OPTIONS_GHC -O0 #-}
-- | This module contains the special Haskell names that are used to map to builtin types or functions
-- in Plutus Core.
--
-- Most users should not use this module directly, but rather use 'PlutusTx.Builtins'.
module PlutusTx.Builtins.Internal where

import Codec.Serialise
import Control.DeepSeq (NFData)
import qualified Data.ByteArray as BA
import Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, empty)
import Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import PlutusTx.Utils (mustBeReplaced)
import Prettyprinter (Pretty (..), viaShow)

{-
We do not use qualified import because the whole module contains off-chain code
which is replaced later with on-chain implementations by the plutus-tx-plugin.
-}
import Prelude as Haskell

{- Note [Builtin name definitions]
The builtins here have definitions so they can be used in off-chain code too.

However they *must* be replaced by the compiler when used in Plutus Tx code, so
in particular they must *not* be inlined, otherwise we can't spot them to replace
them.
-}

{- Note [Delaying error]
The Plutus Core 'error' builtin is of type 'forall a . a', but the
one we expose here is of type 'forall a . () -> a'.

This is because it's hard to get the evaluation order right with
the non-delayed version - it's easy to end up with it getting thrown
unconditionally, or before some other effect (e.g. tracing). On the other
hand, it's much easier to work with the delayed version.

But why not just define that in the library? i.e.

    error = \_ -> Builtins.error

The answer is that GHC is eager to inline and reduce this function, which
does the Wrong Thing. We can't stop GHC doing this (at the moment), but
for most of our functions it's not a *semantic* problem. Here, however,
it is a problem. So we just expose the delayed version as the builtin.
-}

{-# NOINLINE error #-}
error :: BuiltinUnit -> a
error = mustBeReplaced "error"

{-
BOOL
-}

newtype BuiltinBool = BuiltinBool Bool

{-# NOINLINE true #-}
true :: BuiltinBool
true = coerce True

{-# NOINLINE false #-}
false :: BuiltinBool
false = coerce False

{-# NOINLINE ifThenElse #-}
ifThenElse :: BuiltinBool -> a -> a -> a
ifThenElse (BuiltinBool b) x y = if b then x else y

{-
UNIT
-}

newtype BuiltinUnit = BuiltinUnit ()

{-# NOINLINE unitval #-}
unitval :: BuiltinUnit
unitval = BuiltinUnit ()

{-# NOINLINE chooseUnit #-}
chooseUnit :: BuiltinUnit -> a -> a
chooseUnit (BuiltinUnit ()) a = a

{-
INTEGER
-}

type BuiltinInteger = Integer

{-# NOINLINE addInteger #-}
addInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
addInteger = coerce ((+) @Integer)

{-# NOINLINE subtractInteger #-}
subtractInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
subtractInteger = coerce ((-) @Integer)

{-# NOINLINE multiplyInteger #-}
multiplyInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
multiplyInteger = coerce ((*) @Integer)

{-# NOINLINE divideInteger #-}
divideInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
divideInteger = coerce (div @Integer)

{-# NOINLINE modInteger #-}
modInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
modInteger = coerce (mod @Integer)

{-# NOINLINE quotientInteger #-}
quotientInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
quotientInteger = coerce (quot @Integer)

{-# NOINLINE remainderInteger #-}
remainderInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinInteger
remainderInteger = coerce (rem @Integer)

{-# NOINLINE lessThanInteger #-}
lessThanInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinBool
lessThanInteger = coerce ((<) @Integer)

{-# NOINLINE lessThanEqualsInteger #-}
lessThanEqualsInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinBool
lessThanEqualsInteger = coerce ((<=) @Integer)

{-# NOINLINE equalsInteger #-}
equalsInteger :: BuiltinInteger -> BuiltinInteger -> BuiltinBool
equalsInteger = coerce ((==) @Integer)





{-
PAIR
-}

newtype BuiltinPair a b = BuiltinPair (a, b)
    deriving newtype (Haskell.Show, Haskell.Eq, Haskell.Ord)

{-# NOINLINE fst #-}
fst :: BuiltinPair a b -> a
fst (BuiltinPair (a, _)) = a

{-# NOINLINE snd #-}
snd :: BuiltinPair a b -> b
snd (BuiltinPair (_, b)) = b


{-
LIST
-}

newtype BuiltinList a = BuiltinList [a]
    deriving newtype (Haskell.Show, Haskell.Eq, Haskell.Ord)

{-# NOINLINE null #-}
null :: BuiltinList a -> BuiltinBool
null (BuiltinList (_:_)) = coerce False
null (BuiltinList [])    = coerce True

{-# NOINLINE head #-}
head :: BuiltinList a -> a
head (BuiltinList (x:_)) = x
head (BuiltinList [])    = Haskell.error "empty list"

{-# NOINLINE tail #-}
tail :: BuiltinList a -> BuiltinList a
tail (BuiltinList (_:xs)) = coerce xs
tail (BuiltinList [])     = Haskell.error "empty list"

{-# NOINLINE chooseList #-}
chooseList :: BuiltinList a -> b -> b-> b
chooseList (BuiltinList [])    b1 _ = b1
chooseList (BuiltinList (_:_)) _ b2 = b2



{-# NOINLINE mkCons #-}
mkCons :: a -> BuiltinList a -> BuiltinList a
mkCons a (BuiltinList as) = BuiltinList (a:as)


