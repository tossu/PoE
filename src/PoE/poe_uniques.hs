{-# LANGUAGE OverloadedStrings #-}

import Control.Lens (to, only,(^?),ix, toListOf)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, unpack, strip)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Network.HTTP.Client (Response)
import Network.Wreq (responseBody, get)
import Text.Taggy (Node)
import Text.Taggy.Lens (html, element, elements, children, contents, allNamed, attr)
import Meh (readInt, flatMaybeList)

data Item = Item Text Text Int deriving (Show)

table :: [Node] -> Maybe Item
table row = do
    name  <- row ^? ix 0 . allNamed (only "a") . contents
    baseItem  <- row ^? ix 1 . allNamed (only "a") . contents
    level <- fmap (readInt . strip) $ row ^? ix 2 . contents
    return $ Item name baseItem level

items :: Response ByteString -> [Item]
items x = flatMaybeList $ (toListOf $ responseBody . to (decodeUtf8With lenientDecode)
                . html . allNamed (only "tr") . children . to table) x

main = do
    r <- get "http://pathofexile.gamepedia.com/List_of_unique_armour"
    print $ items r
