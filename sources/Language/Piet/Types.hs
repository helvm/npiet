{-# LANGUAGE CPP #-}

-- | A module providing a couple of Piet-specific types
-- and simple associated functions needed throughout the library.
module Language.Piet.Types
	(
	-- * Piet Interpreter
	-- ** Direction Pointer and Codel Chooser
	  DirectionPointer(..), addCoordinates, rotate
	, CodelChooser(..), toggle
	-- ** Piet's type system
	, PietType(..)
	-- ** Runtime program representation
	, Program(..), isBlocked

	-- * Colour system
	, Colour(..), rgba2Colour, rgb2Colour
	, HueColour(..), hueChange
	, Lightness(..), lightnessChange

	-- * Images
	, Image, imgWidth, imgHeight, imgInRange, imgNew, imgPixel, imgSetPixel
	, LabelKey, LabelInfo(..), labelSize, addPixel, LabelBorder(..)
	) where

import Data.Array.IArray
import Data.IntMap hiding ((!))
import Data.Monoid
import Data.Ord

-- | The Direction Pointer (DP).
data DirectionPointer
	= DPRight
	| DPDown
	| DPLeft
	| DPUp
	deriving (Show, Read, Eq, Ord, Enum)

-- | Move coordinates by one in the direction of the 'DirectionPointer'.
addCoordinates :: DirectionPointer	-- ^ Direction to move to
	-> Int				-- ^ x-coordinate
	-> Int				-- ^ y-coordinate
	-> (Int, Int)			-- ^ New x-/y-coordinates
addCoordinates DPRight	x y = (x + 1, y)
addCoordinates DPDown	x y = (x, y + 1)
addCoordinates DPLeft	x y = (x - 1, y)
addCoordinates DPUp	x y = (x, y - 1)

-- | Rotate a 'DirectionPointer' clockwise (counter clockwise if the 'Int' is
-- negative) a given number of times. 
rotate :: Int -> DirectionPointer -> DirectionPointer
rotate n dp = let n' = (n + fromEnum dp) `rem` 4
	in toEnum $ if n' < 0 then n' + 4 else n'

-- | The Codel Chooser (CC).
data CodelChooser
	= CCLeft
	| CCRight
	deriving (Show, Read, Eq, Ord, Enum)

-- | Toggle a 'CodelChooser' a given number of times.
toggle :: Int -> CodelChooser -> CodelChooser
toggle n cc = let n' = (n + fromEnum cc) `rem` 2
	in toEnum $ if n' < 0 then n' + 2 else n'

-- | Piet types. Relevant to distinguish in-/output strategies.
data PietType
	= PietNumber
	| PietChar
	deriving (Show, Read, Eq, Ord)

-- | Runtime program representation.
data Program = Program
	{ image	:: Image Colour		-- ^ Original image
	, mask	:: Image LabelKey	-- ^ Labelled image
	, info	:: IntMap LabelInfo	-- ^ Information about the labels
	}

-- | Returns if a given codel in a program is blocked in the Piet
-- sense (which is the case when it is out of the image's range or
-- 'Black').
isBlocked :: Int -> Int -> Program -> Bool
isBlocked x y program = (not (imgInRange x y (image program)))
	|| (Black == imgPixel x y (image program))

-- | The colours that make up a Piet program text.
data Colour
	= Black
	| White
	| Hue {-# UNPACK #-} !Lightness {-# UNPACK #-} !HueColour
	deriving (Show, Read, Eq, Ord)

-- | Converts red\/green\/blue\/alpha values to a 'Colour'. The alpha channel
-- is ignored for now, but may be used in future implementations or
-- dialects, so please use this function instead of 'rgb2Colour' whenever
-- an alpha channel is available.
rgba2Colour :: Num w => w	-- ^ red
	-> w			-- ^ green
	-> w			-- ^ blue
	-> w			-- ^ alpha
	-> Colour
rgba2Colour r g b _ = rgb2Colour r g b

-- | Converts red\/green\/blue values to a 'Colour'. If the supplied
-- arguments do not form a proper Piet 'Colour', 'White' is returned.
rgb2Colour :: Num w => w	-- ^ red
	-> w		-- ^ green
	-> w		-- ^ blue
	-> Colour
rgb2Colour 0x00 0x00 0x00 = Black
rgb2Colour 0xff 0xff 0xff = White
rgb2Colour 0xff 0xc0 0xc0 = Hue Light  Red
rgb2Colour 0xff 0x00 0x00 = Hue Normal Red
rgb2Colour 0xc0 0x00 0x00 = Hue Dark   Red
rgb2Colour 0xff 0xff 0xc0 = Hue Light  Yellow
rgb2Colour 0xff 0xff 0x00 = Hue Normal Yellow
rgb2Colour 0xc0 0xc0 0x00 = Hue Dark   Yellow
rgb2Colour 0xc0 0xff 0xc0 = Hue Light  Green
rgb2Colour 0x00 0xff 0x00 = Hue Normal Green
rgb2Colour 0x00 0xc0 0x00 = Hue Dark   Green
rgb2Colour 0xc0 0xff 0xff = Hue Light  Cyan
rgb2Colour 0x00 0xff 0xff = Hue Normal Cyan
rgb2Colour 0x00 0xc0 0xc0 = Hue Dark   Cyan
rgb2Colour 0xc0 0xc0 0xff = Hue Light  Blue
rgb2Colour 0x00 0x00 0xff = Hue Normal Blue
rgb2Colour 0x00 0x00 0xc0 = Hue Dark   Blue
rgb2Colour 0xff 0xc0 0xff = Hue Light  Magenta
rgb2Colour 0xff 0x00 0xff = Hue Normal Magenta
rgb2Colour 0xc0 0x00 0xc0 = Hue Dark   Magenta
rgb2Colour _    _    _    = White

-- | Piet colours in the hue cycle.
data HueColour
	= Red
	| Yellow
	| Green
	| Cyan
	| Blue
	| Magenta
	deriving (Show, Read, Eq, Ord, Enum)

-- | Hue difference between two 'HueColour's. 'Red' means no change,
-- 'Yellow' one step and so forth.
hueChange :: HueColour -> HueColour -> HueColour
hueChange c1 c2 = toEnum $ (fromEnum c2 - fromEnum c1) `mod` 6

-- | Hue lightness values supported by Piet.
data Lightness
	= Light
	| Normal
	| Dark
	deriving (Show, Read, Eq, Ord, Enum)

-- | Lightness difference between Piet lightness values. 'Light'
-- represents no change, 'Normal' one step darker and 'Dark'
-- two steps darker.
lightnessChange :: Lightness -> Lightness -> Lightness
lightnessChange l1 l2 = toEnum $ (fromEnum l2 - fromEnum l1) `mod` 3

-- | An image. Its coordinates will be @(0, 0) .. (width-1, height-1)@
data Image a = Image
	{ imgWidth	:: {-# UNPACK #-} !Int		-- ^ Width of an 'Image' in pixels.
	, imgHeight	:: {-# UNPACK #-} !Int		-- ^ Height of an 'Image' in pixels.
	, imgPixels	:: !(Array (Int, Int) a)	-- ^ An 'Array' storing the pixels of an 'Image'.
	}
#ifndef __HADDOCK__
	deriving (Show, Eq, Ord)
#else
instance (Show a) => Show (Image a)
instance (Eq a) => Eq (Image a)
instance (Ord a) => Ord (Image a)
#endif

instance Functor Image where
	fmap f img = img { imgPixels = amap f (imgPixels img) }

-- | Build a new image.
imgNew :: Int			-- ^ Width
	-> Int			-- ^ Height
	-> [((Int, Int), a)]	-- ^ Coordinate-value list
	-> Image a
imgNew width height entries = Image
	{ imgWidth	= width
	, imgHeight	= height
	, imgPixels	= array ((0, 0), (width - 1, height - 1)) entries
	}

-- | Find out, if the given coordinates are within the 'Image'
-- borders (which are @ (0, 0) .. (width-1, height-1)@).
imgInRange :: Int	-- ^ x-coordinate
	-> Int		-- ^ y-coordinate
	-> Image a	-- ^ An 'Image'
	-> Bool		-- ^ If @(x, y)@ is within the 'Image'
imgInRange x y img = 0 <= x && x < imgWidth img && 0 <= y && y < imgHeight img

-- | Access a pixel at given x/y-coordinates.
imgPixel :: Int	-- ^ x-coordinate
	-> Int	-- ^ y-coordinate
	-> Image a -> a
imgPixel x y img = (imgPixels img) ! (x, y)

-- | Set a pixel at given x/y-coordinates.
imgSetPixel :: Int -> Int -> a -> Image a -> Image a
imgSetPixel x y pixel img = img { imgPixels = (imgPixels img) // [((x, y), pixel)] }

-- | We'll just use 'Int's to identifiy labels.
type LabelKey = Int

-- | Stores compiler-relevant information about a label. This type
-- implements an instance of 'Monoid' to merge labels.
data LabelInfo
	= EmptyInfo	-- ^ The empty label
	| LabelInfo
		{ _labelSize	:: {-# UNPACK #-} !Int		-- ^ Number of pixels
		, labelTop	:: {-# UNPACK #-} !LabelBorder	-- ^ Top border
		, labelLeft	:: {-# UNPACK #-} !LabelBorder	-- ^ left border
		, labelBottom	:: {-# UNPACK #-} !LabelBorder	-- ^ Bottom border
		, labelRight	:: {-# UNPACK #-} !LabelBorder	-- ^ Right border
		}	-- ^ Label with a size and four borders
	deriving (Show, Eq, Ord)

-- | Number of pixels in a label. This function is defined for all
-- constructors of 'LabelInfo' so, in contrast to '_labelSize', it
-- won't fail on 'EmptyInfo' .
labelSize :: LabelInfo -> Int
labelSize EmptyInfo		= 0
labelSize _info@(LabelInfo { })	= _labelSize _info

instance Monoid LabelInfo where
	mempty = EmptyInfo

	mappend EmptyInfo i = i
	mappend i EmptyInfo = i
	mappend i1 i2 = LabelInfo
		{ _labelSize	= labelSize i1 + labelSize i2
		, labelTop	= mergeMin (labelTop i1) (labelTop i2)
		, labelLeft	= mergeMin (labelLeft i1) (labelLeft i2)
		, labelBottom	= mergeMax (labelBottom i1) (labelBottom i2)
		, labelRight	= mergeMax (labelRight i1) (labelRight i2)
		}

-- | Holds information of a label (coloured area) relevant for the Piet
-- language, i. e. information about where the program flow will be
-- directed regarding a Direction Pointer.
--
-- Holds a border position (e. g. an x-coordinate) and the minimum
-- or maximum associated \"other\" coordinates (e. g. y-coordinates).
data LabelBorder = LabelBorder
	{ borderCoord	:: {-# UNPACK #-} !Int	-- ^ Where the border is located
	, borderMin	:: {-# UNPACK #-} !Int	-- ^ Minimum \"other\" coordinate of the border
	, borderMax	:: {-# UNPACK #-} !Int	-- ^ Maximum \"other\" coordinate of the border
	} deriving (Show, Eq, Ord)

-- | Merge two 'LabelBorder's holding a /maximum/ coordinate.
mergeMin :: LabelBorder -> LabelBorder -> LabelBorder
mergeMin = merge (comparing borderCoord)

-- | Merge two 'LabelBorder's holding a /minimum/ coordinate.
mergeMax :: LabelBorder -> LabelBorder -> LabelBorder
mergeMax = merge (comparing (negate . borderCoord))

-- | General merge, see 'mergeMin' and 'mergeMax'
merge :: (LabelBorder -> LabelBorder -> Ordering) -> LabelBorder -> LabelBorder -> LabelBorder
merge comp b1 b2 = case comp b1 b2 of
	EQ -> b1
		{ borderMin	= min (borderMin b1) (borderMin b2)
		, borderMax	= max (borderMax b1) (borderMax b2)
		}
	LT -> b1
	GT -> b2

-- | Add a pixel to a 'LabelInfo'.
addPixel :: Int -> Int -> LabelInfo -> LabelInfo
addPixel x y EmptyInfo = LabelInfo
	{ _labelSize	= 1
	, labelTop	= LabelBorder y x x
	, labelLeft	= LabelBorder x y y
	, labelBottom	= LabelBorder y x x
	, labelRight	= LabelBorder x y y
	}
addPixel x y nonEmpty = nonEmpty
	{ _labelSize	= 1 + labelSize nonEmpty
	, labelTop	= mergeMin (labelTop nonEmpty) (LabelBorder y x x)
	, labelLeft	= mergeMin (labelLeft nonEmpty) (LabelBorder x y y)
	, labelBottom	= mergeMax (labelBottom nonEmpty) (LabelBorder y x x)
	, labelRight	= mergeMax (labelRight nonEmpty) (LabelBorder x y y)
	}

