
-- | file: File.Internal.hs
-- | desc: Data types and internal functions for file processing.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module File.Internal (

    DSVHeader
  , DSVFile
  , DSVRecords
  , checkPString

) where

import Data.Char                  (toLower)
import Data.Map.Strict            (Map)
import Data.Vector                (Vector)

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict            as M

-- | DSVRecords are just a vector of bytestring vectors. Each bytestring vector
-- | corresponds to a row in the delimiter separated value (DSV) file. 
--
type DSVRecords = Vector (Vector BS.ByteString)

-- | The header file which is a vector of bytestrings (columns).
--
type DSVHeader = Vector BS.ByteString

-- | The DSV file is a tuple containing the header and all rows.
--
type DSVFile = (DSVHeader, DSVRecords)

-- | These are the list of possible p-value header columns we might find in a file.
-- | Stored as a Map for relatively quick checking.
--
pStringMap :: Map BS.ByteString Bool
--
pStringMap = M.fromList $ zip pstrings $ repeat True
    where
        pstrings = ["p", "pval", "p-val", "p_val", "pvalue", "p-value", "p_value"]

-- | Utility function to see if a string matches one of the p-value header names. Uses
-- | case-insensitive matching.
--
checkPString :: BS.ByteString -> Bool
--
checkPString bs = M.member lowbs pStringMap
    where
        lowbs = BS.map toLower bs

