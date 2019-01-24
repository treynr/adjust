
-- | file: File.hs
-- | desc: File reading, parsing, and writing functions.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module File (

    findPColumn
  , readStatisticsFile
  , writeStatisticsFile

) where

import Data.ByteString.Lex.Fractional (readExponential)
import Data.Char                      (ord, toLower)
import Data.Csv
import Data.Map.Strict                (Map)
import Data.Word                      (Word8)

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector                as V

import File.Internal (DSVRecords, DSVHeader, DSVFile, checkPString)

-- | Sets the delimiter to use when parsing the delimited file.
-- 
decodeDelimiter :: Char -> DecodeOptions
--
decodeDelimiter c = defaultDecodeOptions { decDelimiter = fromIntegral (ord c) }

-- | Sets the delimiter to use when parsing the delimited file.
-- 
encodeDelimiter :: Char -> EncodeOptions
--
encodeDelimiter c = defaultEncodeOptions { encDelimiter = fromIntegral (ord c) }

-- | Uses cassava to parse the delimited file.
--
parseFile :: Char -> BL.ByteString -> Either String DSVRecords
--
parseFile delim = decodeWith (decodeDelimiter delim) NoHeader
    where
        -- | Convert lazy ByteStrings to their strict counterparts
        strictify (Left x) = Left x
        strictify (Right (h, rv)) = 
            Right (V.map BL.toStrict, V.map (V.map BL.toStrict) rv)

-- | Reads and parses the delimited file containing p-values. Returns an Either type
-- | whose Left value is a String in the case that the parsing fails. Otherwise,
-- | the Right value contains the parsed file in the form of a DSVFile.
--
readStatisticsFile :: Char -> FilePath -> IO (Either String DSVFile)
--
readStatisticsFile delim bs = separateHeader . parseFile delim <$> BL.readFile bs
    where
        separateHeader (Left s) = Left s
        separateHeader (Right v) = Right $ (\(h, v) -> (V.head h, v)) $ V.splitAt 1 v

-- | Attempts to figure out what column contains the p-values. Returns the index to 
-- | the correct column, or -1 if it could not be found. If there are multiple p-value
-- | columns then only the last one will be returned.
--
findPColumn :: DSVHeader -> Int
--
findPColumn = V.ifoldl' fold (-1)
    where
        fold ac i b = if checkPString b then i else ac

-- | Writes the updated statistics to a file.
--
writeStatisticsFile :: Char -> FilePath -> DSVFile -> IO ()
--
writeStatisticsFile delim fp (h, rv) = 
    BL.writeFile fp $ encodeWith (encodeDelimiter delim) $ V.toList mergedRecs
    where
        -- | Merge the header and record vector into a single vector
        mergedRecs = V.cons h rv

