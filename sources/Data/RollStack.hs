
-- | This module implements a customized stack for the Piet programming
-- language.
--
-- In addition to the common 'push', 'pop' and 'top' operations, the
-- a 'RollStack' provides a 'roll' (see below) command, which is the
-- reason for using a 'Seq'uence and not a list as underlying data
-- structure.
--
-- Whenever the /O/-notation is used, /n/ describes the number of elements
-- within the stack.
module Data.RollStack
	(
	-- * Stack type
	  RollStack
	
	-- * Query
	, isEmpty
	, top
	
	-- * Construction
	, Data.RollStack.empty
	, Data.RollStack.singleton
	
	-- * Insert delete and roll operations
	, push
	, pop
	, roll

	-- * List conversion
	, Data.RollStack.fromList
	, Data.RollStack.toList
	) where

import Data.Foldable as Foldable
import Data.Maybe
import Data.Sequence  as Seq
import Prelude hiding (foldl, foldr)

-- | The 'RollStack' type.
newtype RollStack a
	= Stack (Seq a)

instance (Show a) => Show (RollStack a) where
	show (Stack xs) = show xs

instance (Eq a) => Eq (RollStack a) where
	(Stack xs) == (Stack ys) = xs == ys

instance (Ord a) => Ord (RollStack a) where
	compare (Stack xs) (Stack ys) = compare xs ys

instance Functor RollStack where
	fmap f (Stack xs) = Stack $ fmap f xs

instance Foldable RollStack where
	fold      (Stack xs) = fold      xs
	foldMap f (Stack xs) = foldMap f xs
	foldr f z (Stack xs) = foldr f z xs
	foldl f z (Stack xs) = foldl f z xs

-- | /O(1)/. Tests, if a 'RollStack' is empty.
isEmpty :: RollStack a -> Bool
isEmpty = isNothing . top

-- | /O(1)/. Looks at the top element of the 'RollStack'. Returns
-- 'Nothing' if the stack is empty.
top :: RollStack a -> Maybe a
top stack = fst `fmap` pop stack

-- | /O(1)/. Construct an empty 'Stack'.
empty :: RollStack a
empty = Stack Seq.empty

-- | /O(1)/. Construct a 'RollStack' containing a single element.
singleton :: a -> RollStack a
singleton = Stack . Seq.singleton

-- | /O(1)/. Push an element on the 'RollStack'.
push :: a -> RollStack a -> RollStack a
push x (Stack xs) = Stack (x <| xs)

-- | /O(1)/. Pop the top element from the 'RollStack' if it is not empty.
pop :: RollStack a -> Maybe (a, RollStack a)
pop (Stack xs) = case viewl xs of
	EmptyL   -> fail "empty RollStack"
	y :< ys -> return (y, Stack ys)

-- | /O(log(n))/. A single roll to depth /n/ is defined as burying the top
-- value on the stack /n/ deep and bringing all values above it up by 1
-- place. A negative number of rolls rolls in the opposite direction. A
-- negative depth results in an 'error'.
roll :: Int		-- ^ Number of rolls
	-> Int		-- ^ Depth
	-> RollStack a	-- ^ Original 'RollStack'
	-> RollStack a	-- ^ Rotated 'RollStack'
roll rolls depth unmodified@(Stack xs)
	| depth < 0                = error $ "negative depth in Data.RollStack.roll: " ++ show depth
	| rolls == 0 || depth == 0 = unmodified
	| otherwise                = let
		(xs1, xs2)   = Seq.splitAt depth xs
		(xs1a, xs1b) = Seq.splitAt (rolls `mod` depth) xs1
		in
		Stack (xs1b >< xs1a >< xs2)

-- | /O(n)/. Convert a list into a 'Stack'. The list's head will be
-- the first element of the 'Stack'
fromList :: [a] -> RollStack a
fromList = Stack . Seq.fromList

-- | /O(n)/. Convert a 'RollStack' to a list. The 'top' of the 'RollStack'
-- will be the list's head.
toList :: RollStack a -> [a]
toList (Stack xs) = Foldable.toList xs

