
-- | file: CmdOptions.hs
-- | desc: Command line option handling and processing.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module CmdOptions (

    Options(..)
  , handleCmdLineArgs
  , scream

) where

import Control.Monad          (when)
import System.Console.CmdArgs ( Data, Typeable, (&=), args, cmdArgs, def
                              , explicit, help, helpArg, isLoud, name, opt, program
                              , summary, typ, verbosity, versionArg
                              )
import System.Environment     (getArgs, withArgs)
import System.Exit            (ExitCode(ExitFailure), exitWith)
import System.IO              (hPutStrLn, stderr)

import Info                   (_DESC, _EXEC, _INFO)

-- | Cmd-line options.
--
data Options = Options {

    -- Convert p-values to adjust q-values
    adjust :: Bool
    -- Control the FWER/FDR at the given alpha 
  , alpha :: Double
    -- Control the FWER using a Bonferroni correction
  , bonferroni :: Bool
    -- Control the FDR using the Benjamini-Hochberg procedure
  , fdr :: Bool
    -- Delimiter to use when parsing the statistics file
  , delim :: String
    -- Column containing the p-value
  , column :: Int
    -- Data file doesn't contain a header row
  , noHeader :: Bool
    -- Remove rows above the given alpha threshold
  , remove :: Bool
    -- Read from stdin instead of a file
  , stdIn :: Bool
    -- Write to stdout instead of a file
  , stdOut :: Bool
    -- Input and output arguments
  , argList :: [FilePath]

} deriving (Data, Eq, Show, Typeable)

-- | Text to display when viewing program options
--
txtAdjust :: String
txtAdjust = "Convert p-values to adjusted p-values, aka q-values"

txtAlpha :: String
txtAlpha = "Control the FWER/FDR at the given alpha (default = 0.05)"

txtBonferroni :: String
txtBonferroni = "Control the FWER using the Bonferroni correction"

txtFdr :: String
txtFdr = "Control the FDR using the Benjamini-Hochberg procedure (default)"

txtColumn :: String
txtColumn = "Zero indexed column that contains the p-value (currently disabled)"

txtDelim :: String
txtDelim = "Delimiter to use when parsing the input statistics file (default = tab)"

txtNoHeader :: String
txtNoHeader = "Input data file doesn't contain a header row (currently disable)"

txtRemove :: String
txtRemove = "Remove rows above the given alpha threshold (useful for large datasets)"

txtStdin :: String
txtStdin = "Read from stdin instead of a file"

txtStdout :: String
txtStdout = "Write to stdout instead of a file"

-- | Fills in info about the program's options.
--
options :: Options
--
options = Options {

    adjust     = False &= explicit &= name "adjust" &= help txtAdjust
  , alpha      = 0.05 &= explicit &= name "a" &= name "alpha" &= 
                 opt (0.05 :: Double) &= help txtAlpha
  , bonferroni = False &= explicit &= name "b" &= name "bonferroni" &= 
                 help txtBonferroni
  , fdr        = False &= explicit &= name "f" &= name "fdr" &= help txtFdr
  , column     = def &= explicit &= name "c" &= name "column" &= help txtColumn
  , delim      = def &= explicit &= name "d" &= name "delim" &= help txtDelim
  , noHeader   = def &= explicit &= name "no-header" &= help txtNoHeader
  , remove     = False &= explicit &= name "r" &= name "remove" &= help txtRemove
  , stdIn      = False &= explicit &= name "i" &= name "stdin" &= help txtStdin
  , stdOut     = False &= explicit &= name "o" &= name "stdout" &= help txtStdout
  , argList    = def &= args &= typ "<input-file> <output-file>"
}

getOptions :: IO Options
--
getOptions = cmdArgs $ options
    -- &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= verbosity
    &= versionArg [explicit, name "version", summary _INFO]
    &= summary (_INFO ++ "\n" ++ _DESC)
    &= help ""
    &= helpArg [explicit, name "help", name "h"]
    &= program _EXEC

-- | Checks to ensure certain user-supplied options and cmd line arguments are set. 
-- | Set default options if they aren't set.
--
checkOptions :: Options -> IO Options
--
checkOptions opts@Options{..}  = do

    when (null argList && not stdIn && not stdOut) $
        putStrLn "ERROR: You must provide an input and output file" >>
        exitWith (ExitFailure 1)

    when (null argList && not stdIn && stdOut) $
        putStrLn "ERROR: You must provide an input file" >>
        exitWith (ExitFailure 1)

    when (null argList && stdIn && not stdOut) $
        putStrLn "ERROR: You must provide an output file" >>
        exitWith (ExitFailure 1)

    when (bonferroni && fdr) $
        putStrLn "ERROR: You can only use on of: --bonferroni, --fdr" >>
        exitWith (ExitFailure 1)

    return opts { 
        delim = if null delim then "\t" else delim
      , fdr   = fdr || not bonferroni 
    }

handleCmdLineArgs = do
    pargs <- getArgs

    -- If the user did not specify any arguments, pretend as "--help" was given
    --
    checkOptions =<< (if null pargs then withArgs ["--help"] else id) getOptions

-- | Output to stderr based on verbosity settings.
--
scream :: String -> IO ()
--
scream s = scream' s =<< isLoud
    where
        scream' _ False = return ()
        scream' s True  = hPutStrLn stderr s
