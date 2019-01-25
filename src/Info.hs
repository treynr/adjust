
-- | file: Info.hs
-- | desc: Program info and constants.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Info (

   _DESC
 , _EXEC
 , _HASH
 , _INFO
 , _VERS

) where

import Development.GitRev (gitHash)

_DESC :: String
_DESC = "Correct for multiple testing by controlling the family-wise error " ++
        "rate (FWER) or the false discovery rate (FDR)" 

_EXEC :: String
_EXEC = "adjust"

_HASH :: String
_HASH = if $(gitHash) == "UNKNOWN" then "" else '-' : take 8 $(gitHash)

_INFO :: String
_INFO = _EXEC ++ " v. " ++ _VERS ++ _HASH

_VERS :: String
_VERS = "1.0.0"

