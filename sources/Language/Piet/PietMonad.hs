{-# LANGUAGE MultiParamTypeClasses #-}

-- | A module implementing the Piet interpreter as a monad. The monad
-- encapsulates the interpreter's status, i. e. the side-effects of
-- Piet programs.
module Language.Piet.PietMonad
	(
	-- * The Piet interpreter monad
	  PietMonad
	, InterpreterStatus
	, LogLevel(..)
	
	-- * Status access
	-- ** Direction Pointer, Codel Chooser and position
	, getDP, setDP
	, getCC, setCC
	, getPosition, setPosition
	-- ** Stack primitives
	, stackPush, stackPop, stackRoll
	
	-- * I/O
	, printNumber, printChar
	, readNumber, readChar
	, logMessage
	
	-- * Termination
	, terminate
	
	-- * Execution
	, runPietMonad
	) where

import Control.Concurrent
import Control.Monad.Error ()
import Control.Monad.State
import Data.RollStack
import Language.Piet.Types

-- | The status of a Piet interpreter.
data InterpreterStatus = InterpreterStatus
	{ dp		:: DirectionPointer	-- ^ Direction Pointer
	, cc		:: CodelChooser		-- ^ Codel Chooser
	, position	:: (Int, Int)		-- ^ Position
	, stack		:: RollStack Int	-- ^ Stack
	}

-- | A request a Piet interpreter may send to it's environment.
data PietRequest
	= Read  PietType	-- ^ The interpreter wants to read from STDIN.
	| Print PietType Int	-- ^ The interpreter wants to print something to STDOUT.
	| Log LogLevel String	-- ^ A log message has been issued.
	| Terminate		-- ^ The Program has terminated.
	deriving (Show, Eq, Ord)

-- | Describes the importance of a log message.
data LogLevel
	= Verbosed	-- ^ Rather verbosed output.
	| Info		-- ^ Usual log level.
	| Error		-- ^ A recoverable error has occured.
	| Fatal		-- ^ A fatal error has occured.
	deriving (Eq, Ord)

instance Show LogLevel where
	show Verbosed	= "VERBOSED"
	show Info	= "INFO    "
	show Error	= "ERROR   "
	show Fatal	= "FATAL   "

-- | A monad encapsulating the status of a Piet interpreter.
newtype PietMonad a = PietMonad (InterpreterStatus
	-> Chan PietRequest
	-> Chan Int
	-> IO (Either String (a, InterpreterStatus)))

instance Monad PietMonad where
	return x = PietMonad (\status _ _ -> return (return (x, status)))
	
	(PietMonad m01) >>= f = PietMonad (\status0 requestChan inputChan -> do
			either1	<- m01 status0 requestChan inputChan
			case either1 of
				Left msg		-> return (fail msg)
				Right (x1, status1)	-> do
					let (PietMonad m12) = f x1
					m12 status1 requestChan inputChan
		)
	
	fail msg = do
		logMessage Fatal msg
		PietMonad (\_ _ _ -> return (fail msg))

instance MonadState InterpreterStatus PietMonad where
	get        = PietMonad $ \status _ _ -> return (return (status, status))
	put status = PietMonad $ \_      _ _ -> return (return ((),     status))

-- | Returns the current Direction Pointer.
getDP :: PietMonad DirectionPointer
getDP = gets dp

-- | Sets the Direction Pointer.
setDP :: DirectionPointer -> PietMonad ()
setDP newDP = modify (\status -> status { dp = newDP })

-- | Returns the current Codel Chooser.
getCC :: PietMonad CodelChooser
getCC = gets cc

-- | Sets the current Codel Chooser.
setCC :: CodelChooser -> PietMonad ()
setCC newCC = modify (\status -> status { cc = newCC })

-- | Returns the current position.
getPosition :: PietMonad (Int, Int)
getPosition = gets position

-- | Sets the current position.
setPosition :: Int -> Int -> PietMonad ()
setPosition x y = modify (\status -> status { position = (x, y) })

-- | Pushes a given 'Int' value on the stack.
stackPush :: Int -> PietMonad ()
stackPush n = modify (\status -> status { stack = push n (stack status) })

-- | Pops the top value from the stack. If the stack was empty,
-- 'Nothing' is returned, 'Just' the top value otherise.
stackPop :: PietMonad (Maybe Int)
stackPop = do
	response <- gets (pop . stack)
	case response of
		Nothing      -> return Nothing
		Just (x, xs) -> do
			modify (\status -> status { stack = xs })
			return (Just x)

-- | Performs the 'roll' operation on the stack.
stackRoll :: Int	-- ^ Roll number
	-> Int		-- ^ Depth
	-> PietMonad ()
stackRoll rolls depth = modify (\status -> status { stack = roll rolls depth (stack status) })

-- | Reads the given 'PietType' from STDIN.
readType :: PietType -> PietMonad Int
readType pType = PietMonad $ \status requestChan inputChan -> do
	writeChan requestChan (Read pType)
	response <- readChan inputChan
	return $ return (response, status)

-- | Reads a number from STDIN.
readNumber :: PietMonad  Int
readNumber = readType PietNumber

-- | Reads a character from STDIN. Note that it is returned as an 'Int'.
readChar :: PietMonad Int
readChar = readType PietChar

-- | Issue log message with given priority.
logMessage :: LogLevel -> String -> PietMonad ()
logMessage level msg = PietMonad $ \status requestChan _ -> do
	writeChan requestChan (Log level msg)
	return $ return ((), status)

-- | Prints a representation of a given 'PietType' to STDOUT.
printType :: PietType -> Int -> PietMonad ()
printType pType n = PietMonad $ \status requestChan _ -> do
	writeChan requestChan (Print pType n)
	return $ return ((), status)

-- | Prints a number to STDOUT.
printNumber :: Int -> PietMonad ()
printNumber = printType PietNumber

-- | Converts a given number to a character and prints it to STDOUT.
printChar :: Int -> PietMonad ()
printChar = printType PietChar

-- | Quit a program. Any command following this one will be ignored.
terminate :: PietMonad ()
terminate = PietMonad $ \status requestChan _ -> do
	writeChan requestChan Terminate
	return $ return ((), status)

-- | Executes a program represented by a 'PietMonad'. I/O operations
-- (reading and writing numbers or characters) is delegated to
-- callback functions.
runPietMonad :: (PietType -> IO Int)		-- ^ Callback to read from STDIN
	-> (PietType -> Int -> IO ())		-- ^ Print callback
	-> (LogLevel -> String -> IO ())	-- ^ Logging callback
	-> PietMonad a				-- ^ The program to be executed
	-> IO (Either String a)			-- ^ Result of the 'PietMonad' or an error message
runPietMonad readCallback printCallback logCallback program = do
	requestChannel	<- newChan
	responseChannel	<- newChan
	lock		<- newEmptyMVar

	-- Fork the actual monadic calculation.

	forkIO $ do
		let PietMonad piet = do
			x <- program

			-- This guarantees, that the service routine (see
			-- below) terminates iff the Piet program terminates,
			-- no matter how broken the program text is
			terminate

			return x

		x <- piet InterpreterStatus
				{ dp		= DPRight
				, cc		= CCLeft
				, position	= (0, 0)
				, stack		= empty
				}
			requestChannel
			responseChannel
		
		putMVar lock x
	
	-- Run the IO part in the current thread. Thus the caller does
	-- not have to worry about thread-safe callbacks as all the IO
	-- stays in the same thread, which is the invoking thread.

	let serviceRoutine = do
		request <- readChan requestChannel
		case request of
			Read pType	-> do
				n <- readCallback pType
				writeChan responseChannel n
				serviceRoutine
			Print pType n	-> do
				printCallback pType n
				serviceRoutine
			Log level msg	-> do
				logCallback level msg
				serviceRoutine
			Terminate	-> return ()

	serviceRoutine
	
	-- Block until the monadic calculation is completed and
	-- return its result.
	
	(liftM fst) `liftM` takeMVar lock 

