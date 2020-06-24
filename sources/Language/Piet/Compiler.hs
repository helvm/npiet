
-- |
-- This module implements the image processing part of this library. It
-- is able to do basic image I/O and provides funcions for labelling
-- images and extracting Piet-relevant information at the same time.
module Language.Piet.Compiler
	(
	-- * I/O
	  imgFromFile
	
	-- * The \"compiler\"
	, compile
	
	-- * Labelling
	, label4, label4With
	) where

import Control.Exception
import Control.Monad
import Data.IntMap hiding (filter)
import Data.List hiding (insert)
import Data.Monoid
import Graphics.Imlib
import Language.Piet.Types

-- | Load an 'Image' holding Piet 'Colour's from a given file.
-- If the codel length is known, it should be passed as 'Just'
-- argument, otherwise, it is guessed from the file. Note that
-- \"codel length\" means the edge length of the codels and
-- not their size.
--
-- /This function is not thread safe due to imlib2!/
imgFromFile :: Maybe Int	-- ^ Codel length or 'Nothing' if unknown
	-> FilePath		-- ^ The image file location
	-> IO (Either ImlibLoadError (Image Colour))
imgFromFile codelInfo file = do
	(img, err)	<- loadImageWithErrorReturn file
	case err of
		ImlibLoadErrorNone	-> bracket
				(contextSetImage img)
				(const freeImageAndDecache)
				$ const $ do
			codelLength	<- maybe imageGuessCodelLength return codelInfo
			img'		<- imageFromContext (max 1 codelLength)
			return (Right img')
		_			-> return (Left err)

-- | Build an @'Image' 'Colour'@ from the current imlib2 context.
imageFromContext :: Int		-- ^ Codel length (not size)
	-> IO (Image Colour)	-- ^ The image data
imageFromContext codelLength = do
	width	<- (`div` codelLength) `liftM` imageGetWidth
	height	<- (`div` codelLength) `liftM` imageGetHeight
	alpha	<- imageHasAlpha

	pixels	<- mapM (\xy@(x, y) -> do
			ImlibColor a r g b <- imageQueryPixel
				(x * codelLength)
				(y * codelLength)
			return (xy, if alpha then rgba2Colour r g b a else rgb2Colour  r g b)
		) [ (x, y) | x <- [ 0 .. width-1 ], y <- [ 0 .. height-1 ] ]

	return $ imgNew width height pixels

-- | Guess the codel length from the image that is currently loaded in
-- the imlib2 buffer. The guess simply is the the gcd of the image
-- width, length and the length of all equally coloured subrows and
-- -cols.
imageGuessCodelLength :: IO Int
imageGuessCodelLength = do
	width	<- imageGetWidth
	height	<- imageGetHeight

	rows	<- mapM (\y -> mapM (\x -> imageQueryPixel x y)
		[ 0 .. width-1 ]) [ 0 .. height-1 ]
	cols	<- mapM (\x -> mapM (\y -> imageQueryPixel x y)
		[ 0 .. height-1 ]) [ 0 .. width-1 ]
	
	return $ lastUntil (==1) $ scanl gcd (gcd width height)
		$ fmap length (group rows) ++ fmap length (group cols)
	
	where

	-- Get the first item of a list that fulfills @p@ or its
	-- last element.
	lastUntil :: Ord a => (a -> Bool) -> [a] -> a
	lastUntil _ [x]    = x
	lastUntil p (x:xs) = if p x then x else lastUntil p xs
	lastUntil _ _      = error "empty list in lastUntil helper (imageGuessCodelLength)"

-- | Compile an @'Image' 'Colour'@ to a Piet 'Program'.
compile :: Image Colour -> Program
compile image_ = let
	(mask_, info_)	= label4 image_
	in
	Program
		{ image	= image_
		, mask	= mask_
		, info	= info_
		}

-- | Status of the labelling algorithm.
data LabellingStatus = LabellingStatus
	{ _currentCoords	:: (Int, Int)		-- ^ Current pixel to investigate
	, _nextKey		:: LabelKey		-- ^ Next unused 'LabelKey'
	, _mask			:: Image LabelKey	-- ^ Each pixel contains a label key
	, _infoMap		:: IntMap LabelInfo	-- ^ Mapping from 'LabelKey's to 'LabelInfo's
	, _equivalences		:: EquivalenceMap	-- ^ Holds information about label equivalences
	} deriving (Show, Eq, Ord)

-- | Label an image with 4-neighbourship and equivalence as neighbouring
-- condition, which is @'label4With' (==)@.
label4 :: Eq a => Image a -> (Image LabelKey, IntMap LabelInfo)
label4 = label4With (==)

-- | Labels an image with 4-neighbourship.
label4With :: (a -> a -> Bool)	-- ^ Decides whether two neighbouring pixels are adjacent.
	-> Image a		-- ^ The 'Image' to be labelled.
	-> (Image LabelKey, IntMap LabelInfo)
				-- ^ A mask 'Image' (containing a key for every pixel)
				-- and a mapping from these keys to 'LabelInfo'.
label4With neighbours img = let
	status	= label4With' neighbours img LabellingStatus
		{ _currentCoords	= (0, 0)
		, _nextKey		= 0
		, _mask			= imgNew (imgWidth img) (imgHeight img) []
		, _infoMap		= mempty
		, _equivalences		= mempty
		}
	img'	= fmap ((flip equivClass) (_equivalences status)) $ _mask status
	inf	= foldWithKey
		(\label labelInfo mergedMap -> let
			label'	= equivClass label $ _equivalences status
			in
			alter (maybe (Just labelInfo) (Just . (mappend labelInfo))) label' mergedMap
		) mempty
		$ _infoMap status
	in (img', inf)

-- | Labelling algorithm expressed in terms of a 'LabellingStatus', see
-- 'label4With'.
label4With' :: (a -> a -> Bool) -> Image a -> LabellingStatus -> LabellingStatus
{-# SPECIALISE
    label4With' :: (Colour -> Colour -> Bool) -> Image Colour -> LabellingStatus -> LabellingStatus
    #-}

label4With' neighbours img status = let
	xy@(x, y)	= _currentCoords status
	pixel		= imgPixel x y img

	mergeLabels	= fmap (\(x', y', _) -> imgPixel x' y' (_mask status))
		$ filter (\(_, _, e) -> neighbours pixel e)
		$ fmap (\(x', y') -> (x', y', imgPixel x' y' img))
		$ previousNeighbours (_currentCoords status)
	
	status'		= case mergeLabels of
		[]		-> let label = _nextKey status in status
			{ _nextKey	= succ label
			, _mask		= imgSetPixel x y label (_mask status)
			, _infoMap	= insert label (addPixel x y mempty) (_infoMap status)
			}
		[label]		-> status
			{ _mask		= imgSetPixel x y label (_mask status)
			, _infoMap	= adjust (addPixel x y) label (_infoMap status)
			}
		[l1, l2]	-> let
			label	= max l1 l2
			in status
				{ _mask		= imgSetPixel x y label (_mask status)
				, _infoMap	= adjust (addPixel x y) label (_infoMap status)
				, _equivalences	= equivInsert l1 l2 (_equivalences status)
				}
		_		-> error
			"too many neighbours in Language.Piet.Compiler.ImageProcessor.label4With'"
	in case nextCoords xy of
		Just xy' -> label4With' neighbours img $ status' { _currentCoords = xy' }
		Nothing  -> status'

	where
	previousNeighbours :: (Int, Int) -> [(Int, Int)]
	previousNeighbours (x, y) = filter (\(x', y') -> x' >= 0 && y' >= 0) [ (x-1, y), (x, y-1) ]

	nextCoords :: (Int, Int) -> Maybe (Int, Int)
	nextCoords (x, y)
		| x < imgWidth img - 1	= Just (x + 1, y)
		| y < imgHeight img - 1	= Just (0, y + 1)
		| otherwise		= Nothing

-- | Detects equivalence classes. Invariant: Every element is mapped to the
-- minimum of it's equivalence class.
type EquivalenceMap = IntMap LabelKey

-- | Find the equivalence class of a given element.
equivClass :: LabelKey -> EquivalenceMap -> LabelKey
equivClass e = findWithDefault e e

-- | Insert a new equivalence.
equivInsert :: LabelKey -> LabelKey -> EquivalenceMap -> EquivalenceMap
equivInsert x y mp = let
	class1		= equivClass x mp
	class2		= equivClass y mp
	classes		= [x, y, class1, class2]
	newClass	= minimum classes
	in
	if x /= y
		then fmap (\eqClass -> if or (fmap (== eqClass) classes) then newClass else eqClass)
			$ insert x newClass
			$ insert y newClass mp
		else mp

