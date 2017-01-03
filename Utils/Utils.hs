{-# LANGUAGE TemplateHaskell, CPP, PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Utils.Utils where
import Utils.TH
import qualified Utils.MakeList as MKL

#define TUPLE_MAX 62
#define TUPLE_SHOW_MAX 15
-- * Manually defined

-- | Tell wether the given value is a fixpoint of the
--   given function.
-- 
-- It holds that
-- 
-- prop> isFixOf (fix f) f = True
--
-- Examples:
--
-- >>> isFixOf () id
-- True
-- >>> isFixOf [1,2,3] sort
-- True
-- >>> isFiXOf [1,3,2] sort
-- False
--
isFixOf :: Eq a =>
	a    	-- ^ The possible fixpoint
	-> (a -> a) -- ^ The function
	-> Bool
isFixOf = flip is

-- | 'isFixOf' with its arguments flipped.
-- 
-- prop> is = flip isFixOf
--
-- Examples:
--
-- >>> is sort [1,2,3]
-- True
-- >>> is sort [1,3,2]
-- False
-- >>> is nub [1,2,3]
-- True
-- >>> is nub [2,2,2]
-- False
-- 
is :: Eq a =>
	(a -> a) -- ^ The function
	-> a    -- ^ The possible fixpoint
	-> Bool
is f x = f x == x

-- | swap the elements with the specified indices.
--   Does nothing if they are out-of-bounds.
{-
swapE :: Integral i => i -> i -> [a] -> [a]
swapE i1 i2 (x:xs)
	| i1 > 0				= x:swapE (pred i1) (pred i2) xs
	| i1 == 0 && genericLength ys1 == i2	= last i2  
-}

-- | Polyvariardic function, generate a 
--   list consisting of all arguments.
mkL :: MKL.RetClass a r => a -> r
mkL = MKL.mkL

list :: [a] -> [a]
list = id

-- * Generated

-- ** curry
-- |
-- prop> curry2 = curry
--
$(genSingle curryN "curry" 2)
$(genNs curryN "curry" [3..7])


-- ** uncurry
-- |
-- prop> uncurry2 = uncurry
--
$(genSingle uncurryN "uncurry" 2)
$(genNs uncurryN "uncurry" [3..7])

-- ** mkT
-- | Build 2-tuple containing the element only.
--
-- >>> mkT2 1
-- (1,1)
--
$(genSingle mkTN "mkT"  2    )
mkT2 :: a		-- ^ The element
	-> (a, a)	-- ^ The resulting tuple
$(genNs     mkTN "mkT" [3..7])

-- ** fmap
-- | Map a function (monomorphically) over all elements of a 2-tuple.
--
-- >>> fmap2 (<3) (1,5)
-- (True, False)
--
$(genSingle fmapN "fmap" 2)
fmap2 :: (a -> b) -- ^ The function to map
	-> (a, a) -- ^ The 2-tuple to be mapped over
	-> (b, b) -- ^ The 2-tuple containg the result after applying the function to both arguments
$(genNs fmapN "fmap" [3..7])

-- ** ap
-- | Apply the argument as the second parameter of the function.
-- Note that the type @b@ can be a function, thus this function will
-- set the second parameter for any function that accepts at least
-- two parameters, for example:
-- 
-- > ap2 [1,2,3] foldl = \ x0 x1 -> foldl x0 [1,2,3] x1
--
-- >>> ap2 1 const 2
-- 2
--
$(genSingle apN "ap" 2)
ap2 :: 	x 			-- ^ The second argument
	-> (a -> x -> b)	-- ^ The function to be applied
	-> (a -> b)
$(genNs apN "ap" [3..7])

-- ** const
-- | Genaterate a constant function with 2 parameters.
--
-- >>> const2 1 () []
-- 1
--
$(genSingle constN "const" 2)
const2 :: 	x 			-- ^ The element to return
		-> _			-- ^ Ignored
		-> _			-- ^ ignored
		-> x			
$(genNs constN "const" [3..7])
