module Meh
(readInt
,flatMaybeList) where

import Data.Text (Text, unpack)

readInt :: Text -> Int
readInt = read . unpack -- NOT SAFE!

flatMaybeList :: [Maybe a] -> [a]
flatMaybeList = foldl f []
    where
        f l Nothing = l
        f l (Just x) = x:l
