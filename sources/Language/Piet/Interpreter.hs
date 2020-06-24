
-- | This module implements an interpreter for the Piet programming
-- language.
module Language.Piet.Interpreter
	(
	-- * Interpreter implementation
	  interpret
	
	-- * Helper functions
	-- ** Movement
	, interpretWhite
	, nonBlackSucc
	, succCoordinates
	-- ** Colour difference handling
	, colours2Command
	, colourDiff2Command
	) where

import Control.Monad
import Data.IntMap hiding (filter)
import Data.Maybe
import Language.Piet.Commands
import Language.Piet.PietMonad
import Language.Piet.Types

-- | Interpret a Piet 'Program'.
interpret :: Program -> PietMonad ()
interpret = interpret' Nothing

-- | Interpret a Piet 'Program'
interpret' :: Maybe (Lightness, HueColour, Int)	-- ^ Previous' block colour and size if it was a hue-block
	-> Program				-- ^ Program
	-> PietMonad ()
interpret' previous program = do
	(x, y)	<- getPosition
	case imgPixel x y (image program) of
		Hue l c	-> do
			maybe (return ())
				(\(oldL, oldC, oldS) -> colours2Command oldL oldC l c oldS)
				previous
			
			dp		<- getDP
			cc		<- getCC
			let key		= imgPixel x y (mask program)
			let label	= findWithDefault EmptyInfo key (info program)
			
			case nonBlackSucc program label dp cc of
				Just (x', y', dp', cc')	-> do
					setPosition x' y'
					setDP dp'
					setCC cc'
					interpret' (Just (l, c, labelSize label)) program
				Nothing			-> do
					terminate
		White	-> interpretWhite program
		Black	-> do
			logMessage Fatal "Entered black block, terminate"
			terminate

-- | Find a way out of the current 'White' block. 'terminate' if there
-- is no way out.
interpretWhite :: Program -> PietMonad ()
interpretWhite program = do
	(x, y)		<- getPosition
	let key		= imgPixel x y (mask program)
	let codels	= labelSize $ findWithDefault EmptyInfo key (info program)
	
	-- The Maximum steps without loop:
	--   every DP/CC combination * size of white block
	-- Is there a better way without actually tracking coordinate/DP/CC tuples?
	
	when (White == imgPixel x y (image program))
		$ interpretWhite' (8 * codels) program

-- | See 'interpretWhite'. The 'Int' determines the maximum remaining
-- moves by one codel and DP/CC changes.
interpretWhite' :: Int -> Program -> PietMonad ()
interpretWhite' limit program
	| limit <= 0	= terminate
	| otherwise	= do
		(x, y)	<- getPosition
		case imgPixel x y (image program) of
			White	-> do
				dp		<- getDP
				let (x', y')	= addCoordinates dp x y
				if isBlocked x' y' program
					then do
						cc	<- getCC
						setDP $ rotate 1 dp
						setCC $ toggle 1 cc
						interpretWhite' (limit - 1) program
					else do
						setPosition x' y'
						interpretWhite' (limit - 1) program
			_	-> interpret' Nothing program -- found a way out

-- | Find coordinates and resulting DP/CC of the successing non-black block,
-- if it exists, 'Nothing' otherwise.
nonBlackSucc :: Program		-- ^ Program
	-> LabelInfo		-- ^ Current block
	-> DirectionPointer	-- ^ DP
	-> CodelChooser		-- ^ CC
	-> Maybe (Int, Int, DirectionPointer, CodelChooser)
				-- ^ Next coordinates, DP and CC (if available)
nonBlackSucc program label dp cc = let
	directions	= fmap (\(r, t) -> (rotate r dp, toggle t cc))
		$ zip [ 0, 0, 1, 1, 2, 2, 3, 3 ] (0 : cycle [ 1, 1, 0, 0 ])
	in
	fmap (\((x, y), (d, c)) -> (x, y, d, c)) $ listToMaybe
		$ filter (\((x, y), _) -> not (isBlocked x y program))
		$ zip (fmap (uncurry (succCoordinates label)) directions) directions

-- | Given a label, a 'DirectionPointer' and a 'CodelChooser', this
-- function finds the coordinates of the next block to enter. These
-- coordinates are not guaranteed to be valid, they might be out of
-- range or point to a 'Black' or 'White' block.
succCoordinates :: LabelInfo	-- ^ Current label
	-> DirectionPointer	-- ^ DP
	-> CodelChooser		-- ^ CC
	-> (Int, Int)		-- ^ Where to enter the next block
succCoordinates label dp cc = let
	(getX, getY) = case (dp, cc) of
		(DPRight, CCLeft)  -> (borderCoord . labelRight, borderMin . labelRight)
		(DPRight, CCRight) -> (borderCoord . labelRight, borderMax . labelRight)
		(DPDown,  CCLeft)  -> (borderMax . labelBottom,  borderCoord . labelBottom)
		(DPDown,  CCRight) -> (borderMin . labelBottom,  borderCoord . labelBottom)
		(DPLeft,  CCLeft)  -> (borderCoord . labelLeft,  borderMax . labelLeft)
		(DPLeft,  CCRight) -> (borderCoord . labelLeft,  borderMin . labelLeft)
		(DPUp,    CCLeft)  -> (borderMin . labelTop,     borderCoord . labelTop)
		(DPUp,    CCRight) -> (borderMax . labelTop,     borderCoord . labelTop)
	in
	addCoordinates dp (getX label) (getY label)

-- | Piet's commands are issued by a colour change, see
-- <http://www.dangermouse.net/esoteric/piet.html>. This function
-- takes two neighbouring colours and returns the resulting Piet
-- command, which is a function consuming (or more likely,
-- ignoring) an 'Int' (the size of the colour block that is being
-- left) and returning a @'PietMonad' ()@.
colours2Command :: Lightness	-- ^ 'Lightness' of the \"from\"-block
	-> HueColour		-- ^ 'HueColour' of the \"from\"-block
	-> Lightness		-- ^ 'Lightness' of the \"to\"-block
	-> HueColour		-- ^ 'HueColour' of the \"to\"-block
	-> Int			-- ^ The size of the \"from\"-block
	-> PietMonad ()		-- ^ Resulting Piet operation
colours2Command fromLight fromColour toLight toColour = colourDiff2Command
	(lightnessChange fromLight toLight) (hueChange fromColour toColour)

-- | Converts a colour difference calculated by 'Language.Piet.Types.colourChange' and
-- 'lightnessChange' to a @'PietMonad' ()@, compare 'colours2Command'.
colourDiff2Command :: Lightness -> HueColour -> Int -> PietMonad ()
colourDiff2Command Light  Red     _ = return ()
colourDiff2Command Normal Red     n = piet_push n
colourDiff2Command Dark   Red     _ = piet_pop
colourDiff2Command Light  Yellow  _ = piet_add
colourDiff2Command Normal Yellow  _ = piet_subtract
colourDiff2Command Dark   Yellow  _ = piet_multiply
colourDiff2Command Light  Green   _ = piet_divide
colourDiff2Command Normal Green   _ = piet_mod
colourDiff2Command Dark   Green   _ = piet_not
colourDiff2Command Light  Cyan    _ = piet_greater
colourDiff2Command Normal Cyan    _ = piet_pointer
colourDiff2Command Dark   Cyan    _ = piet_switch
colourDiff2Command Light  Blue    _ = piet_duplicate
colourDiff2Command Normal Blue    _ = piet_roll
colourDiff2Command Dark   Blue    _ = piet_in_number
colourDiff2Command Light  Magenta _ = piet_in_char
colourDiff2Command Normal Magenta _ = piet_out_number
colourDiff2Command Dark   Magenta _ = piet_out_char

