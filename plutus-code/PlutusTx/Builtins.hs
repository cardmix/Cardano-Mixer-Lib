{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

-- | Primitive names and functions for working with Plutus Core builtins.
module PlutusTx.Builtins (
                                -- * Integer builtins
                                  Integer
                                , addInteger
                                , subtractInteger
                                , multiplyInteger
                                , divideInteger
                                , modInteger
                                , quotientInteger
                                , remainderInteger
                                , greaterThanInteger
                                , greaterThanEqualsInteger
                                , lessThanInteger
                                , lessThanEqualsInteger
                                , equalsInteger
                                -- * Error
                                , error
                                -- * Lists
                                , matchList
                                -- * Conversions
                                , fromBuiltin
                                , toBuiltin
                                ) where

import PlutusTx.Base (const, uncurry)
import PlutusTx.Bool (Bool (..))
import PlutusTx.Builtins.Class
import qualified PlutusTx.Builtins.Internal as BI
import PlutusTx.Integer (Integer)

{-# INLINABLE addInteger #-}
-- | Add two 'Integer's.
addInteger :: Integer -> Integer -> Integer
addInteger x y = fromBuiltin (BI.addInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE subtractInteger #-}
-- | Subtract two 'Integer's.
subtractInteger :: Integer -> Integer -> Integer
subtractInteger x y = fromBuiltin (BI.subtractInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE multiplyInteger #-}
-- | Multiply two 'Integer's.
multiplyInteger :: Integer -> Integer -> Integer
multiplyInteger x y = fromBuiltin (BI.multiplyInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE divideInteger #-}
-- | Divide two integers.
divideInteger :: Integer -> Integer -> Integer
divideInteger x y = fromBuiltin (BI.divideInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE modInteger #-}
-- | Integer modulo operation.
modInteger :: Integer -> Integer -> Integer
modInteger x y = fromBuiltin (BI.modInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE quotientInteger #-}
-- | Quotient of two integers.
quotientInteger :: Integer -> Integer -> Integer
quotientInteger x y = fromBuiltin (BI.quotientInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE remainderInteger #-}
-- | Take the remainder of dividing two 'Integer's.
remainderInteger :: Integer -> Integer -> Integer
remainderInteger x y = fromBuiltin (BI.remainderInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE greaterThanInteger #-}
-- | Check whether one 'Integer' is greater than another.
greaterThanInteger :: Integer -> Integer -> Bool
greaterThanInteger x y = BI.ifThenElse (BI.lessThanEqualsInteger x y ) False True

{-# INLINABLE greaterThanEqualsInteger #-}
-- | Check whether one 'Integer' is greater than or equal to another.
greaterThanEqualsInteger :: Integer -> Integer -> Bool
greaterThanEqualsInteger x y = BI.ifThenElse (BI.lessThanInteger x y) False True

{-# INLINABLE lessThanInteger #-}
-- | Check whether one 'Integer' is less than another.
lessThanInteger :: Integer -> Integer -> Bool
lessThanInteger x y = fromBuiltin (BI.lessThanInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE lessThanEqualsInteger #-}
-- | Check whether one 'Integer' is less than or equal to another.
lessThanEqualsInteger :: Integer -> Integer -> Bool
lessThanEqualsInteger x y = fromBuiltin (BI.lessThanEqualsInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE equalsInteger #-}
-- | Check if two 'Integer's are equal.
equalsInteger :: Integer -> Integer -> Bool
equalsInteger x y = fromBuiltin (BI.equalsInteger (toBuiltin x) (toBuiltin y))

{-# INLINABLE error #-}
-- | Aborts evaluation with an error.
error :: () -> a
error x = BI.error (toBuiltin x)

matchList :: forall a r . BI.BuiltinList a -> r -> (a -> BI.BuiltinList a -> r) -> r
matchList l nilCase consCase = BI.chooseList l (const nilCase) (\_ -> consCase (BI.head l) (BI.tail l)) ()


