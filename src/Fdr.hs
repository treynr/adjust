
-- | file: Fdr.hs
-- | desc: False discovery rate (FDR) calculations.
-- | auth: TR
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Fdr where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lex.Fractional (readExponential)
import Data.Maybe                     (fromMaybe)
import Control.Monad                  (forM, when)
import Data.List                      (foldl', iterate', sortOn)
import Data.Ord                       (comparing)
import Data.Vector                    (Vector, (!), (//))
-- import Data.Vector.Algorithms.Intro   (sortBy)

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS

import File.Internal (DSVRecords)

-- | Attempts to parse a p-value string, formatted as a decimal, into a Double. If a
-- | p-value cannot be parsed, then a value of 1.0 is returned. Probably not the best way
-- | to handle things but easiest.
--
parsePValue :: BS.ByteString -> Double
--
parsePValue = fst . fromMaybe (1.0, "") . readExponential -- . B.toStrict

-- | Extracts p-values from the DSVRecords and converts them into a Double. Returns a
-- | vector of tuples, where the first element is the extracted p-value and the second
-- | element is the original bytestring vector record.
--
extractPValues :: Int -> DSVRecords -> Vector (Double, Vector BS.ByteString)
--
extractPValues dex = V.map (\vs' -> (parsePValue (vs' ! dex), vs'))

-- | Controls the FDR at level alpha using the Benjamini-Hochberg step-up procedure.
--
controlFDR :: Int -> Double -> DSVRecords -> DSVRecords
--
controlFDR dex alpha vs = V.fromList $
                          fmap (snd . snd) $
                          filter (\(i, (p, _)) -> control p i) $
                          zip (iterate' (+1) 1) $
                          sortOn fst $
                          V.toList $ 
                          remove $
                          extractPValues dex vs
    where
        remove      = V.filter (\t -> fst t < alpha)
        size        = V.length vs
        control p k = p <= (fromIntegral k / fromIntegral size) * alpha

-- | Same as the controlFDR function but changes p-values to q-values.
--
adjustFDR :: Int -> Double -> Bool -> DSVRecords -> DSVRecords
--
adjustFDR dex alpha r vs = updateOriginals $
                           remove $
                           V.fromList $
                           updatePValues $
                           sortOn fst $ 
                           V.toList $ 
                           remove $
                           extractPValues dex vs
    where
        remove vs'
            | r = V.filter (\t -> fst t < alpha) vs'
            | otherwise = vs'
        size = V.length vs
        updatePValues pv = foldl' (\ac (i, (p, bv)) -> (control p i, bv) : ac) [] $ 
                           zip [1..] pv
        updateOriginals = V.map (\(p, bv) -> bv // [(dex, BS.pack $ show p)])
        control p k = p * fromIntegral size / fromIntegral k

-- | Controls the FWER at level alpha using the Bonferroni correction.
--
controlFWER :: Int -> Double -> DSVRecords -> DSVRecords
--
controlFWER dex alpha vs = V.map snd $ 
                           remove $ 
                           V.filter (\(p, bv) -> control p) $
                           extractPValues dex vs
    where
        remove    = V.filter (\t -> fst t < alpha)
        size      = V.length vs
        control p = p <= (alpha / fromIntegral size)

-- | Same as the controlFWER function but changes p-values to q-values.
--
adjustFWER :: Int -> Double -> Bool -> DSVRecords -> DSVRecords
--
adjustFWER dex alpha r vs = updateOriginals $
                            remove $
                            updatePValues $
                            remove $
                            extractPValues dex vs
    where
        remove vs'
            | r         = V.filter (\t -> fst t < alpha) vs'
            | otherwise = vs'
        size            = V.length vs
        updatePValues   = V.map (\(p, bv) -> (control p, bv))
        updateOriginals = V.map (\(p, bv) -> bv // [(dex, BS.pack $ show p)])
        control p       = p * fromIntegral size
