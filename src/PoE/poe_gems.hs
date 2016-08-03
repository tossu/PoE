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

data Gem = Gem Text Int GemType GemAttrType deriving (Show)
data GemAttrType = Strength | Dexterity | Intelligence | General deriving (Show)
data GemType = Active | Support deriving (Show)

parseGemType :: Maybe Text -> Maybe GemType
parseGemType Nothing = Nothing
parseGemType (Just s) = case (unpack s) of
                    "gem-link str-skill" -> Just Active
                    "gem-link dex-skill" -> Just Active
                    "gem-link int-skill" -> Just Active
                    "gem-link gen-skill" -> Just Active
                    "gem-link str-suppt" -> Just Support
                    "gem-link dex-suppt" -> Just Support
                    "gem-link int-suppt" -> Just Support
                    "gem-link gen-suppt" -> Just Support
                    _ -> Nothing

parseGemAttrType :: Maybe Text -> Maybe GemAttrType
parseGemAttrType Nothing = Nothing
parseGemAttrType (Just s) = case (unpack s) of
                    "gem-link str-skill" -> Just Strength
                    "gem-link str-suppt" -> Just Strength
                    "gem-link dex-skill" -> Just Dexterity
                    "gem-link dex-suppt" -> Just Dexterity
                    "gem-link int-suppt" -> Just Intelligence
                    "gem-link int-skill" -> Just Intelligence
                    "gem-link gen-suppt" -> Just General
                    "gem-link gen-skill" -> Just General
                    _ -> Nothing

table :: [Node] -> Maybe Gem
table row = do
    classText <- row ^? ix 0 . allNamed (only "span") . attr "class"
    gemType <- parseGemType classText
    gemAttrType <- parseGemAttrType classText
    name  <- row ^? ix 0 . allNamed (only "a") . contents
    level <- fmap (readInt . strip) $ row ^? ix 1 . contents
    return $ Gem name level gemType gemAttrType

gems :: Response ByteString -> [Gem]
gems x = flatMaybeList ((toListOf $ responseBody . to (decodeUtf8With lenientDecode)
                . html . allNamed (only "tr") . children . to table) x)

main = do
    r <- get "http://pathofexile.gamepedia.com/List_of_skills"
    print $ gems r
