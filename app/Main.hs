
-- | file: Main.hs 
-- | desc: Main stuffs: cmd line processing and program execution.
-- | auth: TR
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Applicative      ((<$>))
-- import Control.Exception.Safe (Exception, MonadThrow, SomeException, throwM)
import Control.Monad            (when)
import Data.ByteString.Char8    (ByteString)
import Data.Char (ord)
import Data.List                (intercalate, sortOn)
--import Data.Time                (getCurrentTime, toGregorian, utctDay)
import Data.Typeable            (TypeRep, Typeable, typeRep)
--import Development.GitRev       (gitBranch, gitCommitCount, gitHash)
import System.Console.CmdArgs
import System.Environment       (getArgs, withArgs)
import System.Exit
--import System.Exit            (ExitCode(ExitFailure), exitWith)

import CmdOptions (Options(..), handleCmdLineArgs, scream)
--import Exceptions
import Fdr
import File.Internal            (DSVFile)
import File

import qualified Data.Vector as V

main :: IO ()
--
main = exec =<< handleCmdLineArgs
    --do
    --pargs <- getArgs
    ---- If the user did not specify any arguments, pretend as "--help" was given
    --opts <- (if null pargs then withArgs ["--help"] else id) getOptions
    --exec <$> checkOptions opts
    --exec <$> handleCmdLineArgs
    
 
{-
-- | Produces FDR adjusted p-values (q-values) using the adjust procedure
-- | outlined in Yekutieli & Benjamini (1999) eq. 3 .
--
adjustFdr :: [(Double, [ByteString])] -> [(Double, [ByteString])]
--
adjustFdr bs = adjust sortedList
    where
        size = length bs
        sortedList = zipWith (\a b -> (a, fst b, snd b)) [1..size] (sortOn fst bs)
        q (i, p, _) = (p * fromIntegral size) / fromIntegral i
        adjust [] = []
        adjust pss@((_, _, b):ps) = (mapAndMin pss, b) : adjust ps
        mapAndMin (t : []) = q t
        mapAndMin (t1 : t2 : ps)
            | q t1 == q t2 = mapAndMin (t2 : ps)
            | q t1 < q t2 = q t1
            | q t1 > q t2 = q t2

controlFdr :: Double -> [(Double, [ByteString])] -> [(Double, [ByteString])]
--
controlFdr a bs = fmap (\(_, p, b) -> (p, b)) $ 
                  filter (\(k, p, _) -> control p k) $ mapIndices $ 
                  sortOn fst bs
    where
        size = length bs
        mapIndices = zipWith (\a b -> (a, fst b, snd b)) [1..size] 
        control p k = p <= (fromIntegral k / fromIntegral size) * a

handleFilterOptions :: Options -> 
                       [(Double, [ByteString])] -> 
                       [(Double, [ByteString])]
--
handleFilterOptions Options{..} bs
    | adjust = adjustFdr bs
    | fdr > 0.0 = controlFdr fdr bs
    | otherwise = bs

-}

handleInputFile :: Either String a -> a
--
handleInputFile (Left s) = error "Failed to read input file"
handleInputFile (Right s) = s

handlePColumn :: Int -> Int
--
handlePColumn (-1) = error "Failed to find the p-value column"
handlePColumn x = x

-- | Handles multiple testing correction based on user supplied options and returns
-- | the corrected list of p-values.
--
controlErrors :: Options -> Int -> DSVFile -> DSVFile
--
controlErrors Options{..} dex df
    | fdr && adjust        = (\(h, rv) -> (h, adjustFDR dex alpha remove rv)) df
    | fdr                  = (\(h, rv) -> (h, controlFDR dex alpha rv)) df
    | bonferroni && adjust = (\(h, rv) -> (h, adjustFWER dex alpha remove rv)) df
    | bonferroni           = (\(h, rv) -> (h, controlFWER dex alpha rv)) df

---- Where all the execution magic happens. 
--
exec :: Options -> IO ()
--
exec opts@Options{..} = do 

    -- Verbosity argument
    verb <- isLoud

    let input = argList !! 0
    let output = argList !! 1

    -- | Attempt to read in the input file, head is bad but delim should
    -- | never be an empty list.
    records <- handleInputFile <$> readStatisticsFile (head delim) input

    -- | Attempt to find the index for the p-value column
    let pColumn = handlePColumn $ findPColumn $ fst records

    -- | Control FWER/FDR based on user settings
    let records' = controlErrors opts pColumn records

    writeStatisticsFile '\t' output records'

