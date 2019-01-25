
-- | file: Main.hs 
-- | desc: Main stuffs: cmd line processing and program execution.
-- | auth: TR
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Main where

import System.IO     (IOMode(ReadMode, WriteMode), stdin, stdout, withFile)

import CmdOptions    (Options(..), handleCmdLineArgs)
import Fdr           (adjustFDR, controlFDR, adjustFWER, controlFWER)
import File.Internal (DSVFile)
import File          ( findPColumn, readStatisticsFile, readStatisticsFile'
                     , writeStatisticsFile'
                     )

main :: IO ()
--
main = exec =<< handleCmdLineArgs

-- | Handles multiple testing correction based on user supplied options and returns
-- | the corrected list of p-values.
--
controlErrors :: Options -> Int -> DSVFile -> DSVFile
--
controlErrors Options{..} dex df
    | fdr && adjust  = (\(h, rv) -> (h, adjustFDR dex alpha remove rv)) df
    | fdr            = (\(h, rv) -> (h, controlFDR dex alpha rv)) df
    | fwer && adjust = (\(h, rv) -> (h, adjustFWER dex alpha remove rv)) df
    | fwer           = (\(h, rv) -> (h, controlFWER dex alpha rv)) df

handlePColumn :: Int -> Int
--
handlePColumn (-1) = error "Failed to find the p-value column"
handlePColumn x = x

-- | Handles input options to either read from stdin or a file and return the parsed
-- | delimited file.
--
handleInput :: Options -> IO DSVFile
--
handleInput Options{..}
    | stdIn     = eitherError <$> readStatisticsFile' (head delim) stdin
    | otherwise = eitherError <$> readStatisticsFile (head delim) (argList !! 0)
        
    where
        -- | Returns an error if the file couldn't be parsed otherwise removes the Either
        -- | type from the DSVFile. Error is bad but it's easy.
        eitherError (Left _) = error "Failed to read input file"
        eitherError (Right f) = f

-- | Handles output options to either output corrected statistics to stdout or an output
-- | file.
--
handleOutput :: Options -> DSVFile -> IO ()
--
handleOutput Options{..} dsv
    | stdIn && stdOut     = writeStatisticsFile' (head delim) stdout dsv
    | not stdIn && stdOut = writeStatisticsFile' (head delim) stdout dsv
    | stdIn && not stdOut = withFile (argList !! 0) WriteMode $ \handle ->
        writeStatisticsFile' (head delim) handle dsv
    | otherwise           = withFile (argList !! 1) WriteMode $ \handle ->
        writeStatisticsFile' (head delim) handle dsv

---- Where all the execution magic happens. 
--
exec :: Options -> IO ()
--
exec opts@Options{..} = do 

    -- | Attempt to read in the input 
    records <- handleInput opts

    -- | Attempt to find the index for the p-value column
    let pColumn = handlePColumn $ findPColumn $ fst records

    -- | Control FWER/FDR based on user settings
    let records' = controlErrors opts pColumn records

    handleOutput opts records'

