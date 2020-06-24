module Main
	( main
	) where

import Control.Monad.Error
import Data.List
import Data.Version (showVersion)
import Language.Piet
import qualified Paths_piet as Paths (version)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

-- | Main function.
main :: IO ()
main = do
	args	<- getArgs
	case parseArguments args of
		Left err	-> do
			putStrLn err
			printUsage
			exitWith $ ExitFailure 1
		Right opts	-> do
			actualMain opts
			exitWith ExitSuccess

-- | Main function without the option-parsing part.
actualMain :: Options -> IO ()
actualMain Help		= printUsage
actualMain Version	= printVersion
actualMain opts@Run { }	= case program opts of
	Nothing		-> do
		putStrLn "No program given"
		printUsage
	Just file	-> do
		loadresult	<- imgFromFile (codelSize opts) file
		case loadresult of
			Left err	-> do
				putStr "Unable to load image, imlib2 issued this error: "
				print err
				exitWith $ ExitFailure 2
			Right img	-> do
				let prog	= compile img

				hSetBuffering stdin  NoBuffering
				hSetBuffering stdout NoBuffering
				hFlush stdout
				
				response	<- runPietMonad readCallback
					printCallback
					(logCallback (verbosity opts))
					(interpret prog)

				case response of
					Left err	-> do
						putStrLn err
						exitWith $ ExitFailure 3
					Right _		-> return ()

-- | Print program usage to STDOUT.
printUsage :: IO ()
printUsage = getProgName >>= putStrLn . ((flip usageInfo) options) . versionise

-- | Print program name and version to STDOUT.
printVersion :: IO ()
printVersion = getProgName >>= putStrLn . versionise

-- | Append /-version/ to a program name, where /version/ is
-- the actual version of this cabal package.
versionise :: String -> String
versionise = (++ "-" ++ showVersion Paths.version)

-- | Callback function to read a 'PietNumber' (i. e. a parsed line) or a
-- 'PietChar' from STDIN.
readCallback :: PietType -> IO Int
readCallback PietNumber = getLine >>= maybe (readCallback PietNumber) return . readMaybe
readCallback PietChar   = fromEnum `liftM` getChar

-- | Callback to print a 'PietType'.
printCallback :: PietType -> Int -> IO ()
printCallback PietNumber = putStr . show
printCallback PietChar   = putChar . toEnum

-- | Callback to manage log messages.
logCallback :: LogLevel -> LogLevel -> String -> IO ()
logCallback threshold level msg = when (level >= threshold) $ do
	let prefixedMsg = show level ++ ' ' : msg
	putStrLn prefixedMsg

-- | Build 'Options' and - if specified - a program file to execute
-- from a list of 'String's (the arguments).
parseArguments :: [String] -> Either String Options
parseArguments args = do
	let (optCBs, remaining, errors)	= getOpt Permute options args

	unless (null errors) $ fail (unlines errors)
	opts	<- foldM (flip ($)) defaultOptions optCBs
	
	case remaining of
		[]	-> return opts
		[prog]	-> case opts of
			Run { }	-> return (opts { program = Just prog } )
			_	-> return opts
		_	-> fail "please specify exactly one program"

-- | Type bundling command line options.
data Options
	= Help		-- ^ Don't run anything, just print a help text
	| Version	-- ^ Don't run anything, print program name and version
	| Run
		{ program	:: Maybe FilePath	-- ^ Program to run
		, codelSize	:: Maybe Int		-- ^ Explicitly specify codel size
		, verbosity	:: LogLevel
		}	-- ^ Run a given program
	deriving (Show, Eq, Ord)

-- | The default 'Options'.
defaultOptions :: Options
defaultOptions = Run
	{ program	= Nothing
	, codelSize	= Nothing
	, verbosity	= Info
	}

-- | List of command line options accepted by this interpreter.
options :: [OptDescr (Options -> (Either String Options))]
options =
	[ Option ['h', '?'] ["help"]
		(NoArg (const (Right Help)))
		"print usage information and exit"
	, Option [] ["version"]
		(NoArg (const (Right Version)))
		"print version information and exit"
	, Option ['c'] ["codels"]
		(ReqArg (\arg opt -> maybe
			(Left "codel size must be a number")
			(\x -> case opt of
				Run { }	-> Right (opt { codelSize = Just x })
				_	-> Right opt)
			(readMaybe arg)) "LENGTH")
		"codel length (the codel size will be LENGTH^2)"
	, Option ['v'] ["verbose"]
		(NoArg (\opt -> case opt of
			Run { }	-> Right (opt { verbosity = Verbosed } )
			_	-> Right opt))
		"give verbosed output"
	, Option ['q'] ["quiet"]
		(NoArg (\opt -> case opt of
			Run { }	-> Right (opt { verbosity = Error } )
			_	-> Right opt))
		"suppress all but error messages"
	]

-- | Like 'read', but returns 'Nothing' instead of raising an 'error' on failure.
readMaybe :: Read a => String -> Maybe a
readMaybe str = find ((== "") . snd) (reads str) >>= return . fst

