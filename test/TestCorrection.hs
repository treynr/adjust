
-- | file: TestCorrection.hs
-- | desc: Unit testing for the functions in the Correction module.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCorrection where

import Data.Vector
import Test.Tasty
import Test.Tasty.HUnit

import Correction
import File

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V

test_Correction = testGroup "Correction.hs unit tests" tests

tests = [ test_controlFDR
        , test_adjustFDR
        , test_controlFWER
        , test_adjustFWER
        ]

records = (\(Right x) -> x) <$> readStatisticsFile '\t' "test/data/sample2.txt"

test_controlFDR = testCase "controlFDR" $ do
    header <- fst <$> records
    recs <- snd <$> records
    (V.length $ controlFDR 0 0.05 recs) @?= 5

test_adjustFDR = testCase "adjustFDR" $ do
    header <- fst <$> records
    recs <- snd <$> records
    let adjust = adjustFDR 0 0.05 True recs

    V.length adjust @?= 5
    
    -- This is so gross
    (read $ B.unpack ((adjust ! 0) ! 0) :: Double) @?= 0.04
    (read $ B.unpack ((adjust ! 1) ! 0) :: Double) @?= 0.025
    (read $ B.unpack ((adjust ! 2) ! 0) :: Double) @?= 0.03333333333333333
    (read $ B.unpack ((adjust ! 3) ! 0) :: Double) @?= 0.01
    (read $ B.unpack ((adjust ! 4) ! 0) :: Double) @?= 0.01

test_controlFWER = testCase "controlFWER" $ do
    header <- fst <$> records
    recs <- snd <$> records
    (V.length $ controlFWER 0 0.05 recs) @?= 2

test_adjustFWER = testCase "adjustFWER" $ do
    header <- fst <$> records
    recs <- snd <$> records
    let adjust = adjustFWER 0 0.05 True recs

    V.length adjust @?= 2
    
    -- This is so gross
    (read $ B.unpack ((adjust ! 0) ! 0) :: Double) @?= 0.02
    (read $ B.unpack ((adjust ! 1) ! 0) :: Double) @?= 0.01
