
-- | file: TestFile.hs
-- | desc: Unit testing for the functions in the File module.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestFile where

import Data.Vector
import Test.Tasty
import Test.Tasty.HUnit

import File.Internal
import File

import qualified Data.Vector as V

test_File = testGroup "File.hs unit tests" tests

tests = [ test_checkPString_1 
        , test_checkPString_2
        , test_findPColumn
        , test_readStatisticsFile_1
        , test_readStatisticsFile_2
        ]

test_checkPString_1 = testCase "checkPString #1" $ do
    checkPString "p" @?= True
    checkPString "P" @?= True
    checkPString "PVAL" @?= True
    checkPString "Pval" @?= True
    checkPString "P-val" @?= True
    checkPString "P_VAL" @?= True
    checkPString "pvalue" @?= True
    checkPString "pVALUE" @?= True
    checkPString "p_VALue" @?= True
    checkPString "p-value" @?= True

test_checkPString_2 = testCase "checkPString #2" $ do
    checkPString "q" @?= False
    checkPString "Pv" @?= False
    checkPString "p value" @?= False
    checkPString "pp" @?= False

test_findPColumn = testCase "findPColumn" $ do
    findPColumn (V.fromList ["p", "q", "info"]) @?= 0
    findPColumn (V.fromList ["p", "pval", "info"]) @?= 1
    findPColumn (V.fromList ["beta", "p-VALUE", "info"]) @?= 1
    findPColumn (V.fromList ["beta", "q", "P-VAL"]) @?= 2

    findPColumn (V.fromList ["ps", "q", "info"]) @?= -1
    findPColumn (V.fromList ["px", "pz", "info"]) @?= -1
    findPColumn (V.fromList ["beta", "p-VALUESS", "info"]) @?= -1
    findPColumn (V.fromList ["beta", "q", "P-V"]) @?= -1

test_readStatisticsFile_1 = testCase "readStatisticsFile #1" $ do
    fl <- (\(Right x) -> x) <$> readStatisticsFile '\t' "test/data/sample1.txt"

    (findPColumn $ fst fl) @?= 10
    (V.length $ snd fl) @?= 99999

test_readStatisticsFile_2 = testCase "readStatisticsFile #2" $ do
    fl <- (\(Right x) -> x) <$> readStatisticsFile '\t' "test/data/sample2.txt"

    (findPColumn $ fst fl) @?= 0
    (V.length $ snd fl) @?= 10

