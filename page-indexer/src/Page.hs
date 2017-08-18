module Page
  ( Page(..)
  , Tag
  ) where

import Data.Aeson
import Data.Aeson.TH

type Tag = String

data Page = Page
  { url :: String
  , tags :: [Tag]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Page)