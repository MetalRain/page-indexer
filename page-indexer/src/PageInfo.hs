module PageInfo
  ( PageInfo(..)
  ) where

import Data.Aeson
import Data.Aeson.TH

data PageInfo = PageInfo
  { url :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''PageInfo)