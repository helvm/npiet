
-- | This module contains the implementation of the Piet language
-- constructs. Most of the documentation is copied from the
-- Piet specification at <http://www.dangermouse.net/esoteric/piet.html>.
module Language.Piet.Commands
	(
	-- * Stack access
	  piet_push
	, piet_pop
	
	-- * Arithmetic operators
	, piet_add
	, piet_subtract
	, piet_multiply
	, piet_divide
	, piet_mod
	
	-- * Boolean operations
	, piet_not
	, piet_greater
	
	-- * Movement
	, piet_pointer
	, piet_switch
	
	-- * Stack modification
	, piet_duplicate
	, piet_roll
	
	-- * I/O
	, piet_in_number, piet_in_char
	, piet_out_number, piet_out_char
	) where

import Control.Monad
import Language.Piet.PietMonad
import Language.Piet.Types

-- | Pushes the value of the colour block just exited on to the stack.
-- Note that values of colour blocks are not automatically pushed on
-- to the stack - this push operation must be explicitly carried out.
piet_push :: Int -> PietMonad ()
piet_push n = do
	logWithPosition $ "push " ++ show n
	stackPush n

-- | Pops the top value off the stack and discards it.
piet_pop :: PietMonad ()
piet_pop = do
	logWithPosition "pop"
	forcePopFail "pop"
	return ()

-- | Pops the top two values off the stack, adds them, and pushes the
-- result back on the stack.
piet_add :: PietMonad ()
piet_add = do
	logWithPosition "add"
	onStack2 "add" (+)

-- | Pops the top two values off the stack, subtracts the top value from
-- the second top value, and pushes the result back on the stack.
piet_subtract :: PietMonad ()
piet_subtract = do
	logWithPosition "subtract"
	onStack2 "subtract" (flip (-))

-- | Pops the top two values off the stack, multiplies them, and pushes the
-- result back on the stack. 
piet_multiply :: PietMonad ()
piet_multiply = do
	logWithPosition "multiply"
	onStack2 "multiply" (*)

-- | Pops the top two values off the stack, calculates the integer division
-- of the second top value by the top value, and pushes the result back on
-- the stack.
piet_divide :: PietMonad ()
piet_divide = do
	logWithPosition "divide"
	onStack2 "divide" (flip div)

-- | Pops the top two values off the stack, calculates the second top value
-- modulo the top value, and pushes the result back on the stack.
piet_mod :: PietMonad ()
piet_mod = do
	logWithPosition "mod"
	onStack2 "mod" (flip Prelude.mod)

-- | Replaces the top value of the stack with 0 if it is non-zero, and 1 if
-- it is zero.
piet_not :: PietMonad ()
piet_not = do
	logWithPosition "not"
	onStack1 "not" not'
	where
	not' 0 = 1
	not' _ = 0

-- | Pops the top two values off the stack, and pushes 1 on to the stack if
-- the second top value is greater than the top value, and pushes 0 if it
-- is not greater.
piet_greater :: PietMonad ()
piet_greater = do
	logWithPosition "greater"
	onStack2 "greater" greater'
	where
	greater' a b
		| a < b     = 1
		| otherwise = 0

-- | Pops the top value off the stack and rotates the DP clockwise that many
-- steps (anticlockwise if negative).
piet_pointer :: PietMonad ()
piet_pointer = do
	n	<- forcePopFail "pointer"
	dp	<- getDP
	let dp'	= (rotate n dp)
	setDP dp'
	logWithPosition $ "pointer " ++ show dp'

-- | Pops the top value off the stack and toggles the CC that many times.
piet_switch :: PietMonad ()
piet_switch = do
	n	<- forcePopFail "switch"
	cc	<- getCC
	let cc'	= toggle n cc
	setCC cc'
	logWithPosition $ "switch " ++ show cc'

-- | Pushes a copy of the top value on the stack on to the stack.
piet_duplicate :: PietMonad ()
piet_duplicate = do
	logWithPosition "duplicate"
	x <- forcePopFail "duplicate"
	stackPush x
	stackPush x

-- | Pops the top two values off the stack and \"rolls\" the remaining stack
-- entries to a depth equal to the second value popped, by a number of
-- rolls equal to the first value popped. A single roll to depth /n/ is
-- defined as burying the top value on the stack /n/ deep and bringing all
-- values above it up by 1 place. A negative number of rolls rolls in the
-- opposite direction. A negative depth is an error and the command is
-- ignored.
--
-- In this implementation, \"ignored\" means that the top two values
-- remain pushed off the stack, while the rest of the stack remains
-- unmodified.
piet_roll :: PietMonad ()
piet_roll = do
	logWithPosition "roll"
	rolls <- forcePopFail "roll"
	depth <- forcePopFail "roll"
	when (depth > 0 && rolls /= 0) $ stackRoll rolls depth

-- | Reads a number from STDIN and pushes it on to the stack. 
piet_in_number :: PietMonad ()
piet_in_number = do
	logWithPosition "in_number"
	n <- readNumber
	stackPush n

-- | Reads a char from STDIN and pushes it on to the stack.
piet_in_char :: PietMonad ()
piet_in_char = do
	logWithPosition "in_char"
	n <- readChar
	stackPush n

-- | Pops the top value off the stack and prints it to STDOUT
-- as a number.
piet_out_number :: PietMonad ()
piet_out_number = do
	n <- forcePopFail "out_number"
	logWithPosition $ "out_number " ++ show n
	printNumber n

-- | Pops the top value off the stack and prints it to STDOUT
-- as a char.
piet_out_char :: PietMonad ()
piet_out_char = do
	n <- forcePopFail "out_char"
	logWithPosition $ "out_char " ++ show n
	printChar n

-- | Pops the top element of the stack, applies a function to it
-- and pushes the result back on the stack. The 'String' describes
-- the calling Piet function for possible errors.
onStack1 :: String -> (Int -> Int) -> PietMonad ()
onStack1 location f = liftM f (forcePopFail location) >>= stackPush

-- | Pops the top two elements of the stack, applies a function to
-- them (the first argument will be the first element popped from
-- the stack, the 2nd will be the 2nd) and pushes the result back
-- on the stack. The 'String' describes the calling Piet function
-- and might be needed to give error messages.
onStack2 :: String -> (Int -> Int -> Int) -> PietMonad ()
onStack2 location f = liftM2 f (forcePopFail location) (forcePopFail location) >>= stackPush

-- | Pops the top element from the stack and 'fail's if none is
-- available (with the given 'String' as location).
forcePopFail :: String -> PietMonad Int
forcePopFail location = forcePop (fail $ "Empty stack at " ++ location)

-- | Tries to pop the top element from the stack and returns it.
-- If the stack is empty, the alternative action is performed.
forcePop :: PietMonad Int -- ^ Will be executed if the stack is empty.
	-> PietMonad Int  -- ^ Returns the top stack entry otherwise.
forcePop errorAction = stackPop >>= maybe errorAction return

-- | Helper that issues a 'Verbosed' log message and prefixes
-- it with the current position.
logWithPosition :: String -> PietMonad ()
logWithPosition msg = do
	pos	<- getPosition
	logMessage Verbosed $ show pos ++ ' ' : msg

