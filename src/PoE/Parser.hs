{-# LANGUAGE OverloadedStrings #-}

module PoE.Parser where

import Text.Parsec
import Text.Parsec.String
import System.Environment

data Color
    = RGB Int Int Int
    | RGBA Int Int Int Int
    deriving (Show, Read)

data ActionAttribute
    = SetBorderColor
    | SetTextColor
    | SetBackgroundColor
    | PlayAlertSound
    | SetFontSize
    deriving (Show, Read)

data ActionValue
    = Color Color
    | FontSize Int
    | Sound Int Int
    deriving (Show)

data LimitAttribute
    = ItemLevel
    | DropLevel
    | Quality
    | Rarity
    | Class
    | BaseType
    | Sockets
    | LinkedSockets
    | SocketGroup
    | Height
    | Width
    deriving (Show, Read)

data Operator
    = LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equal
    deriving (Show)

data LimitValue
    = Number Int
    | Text String
    | Normal -- TODO: Rarity in own type?
    | Magic
    | Rare
    | Unique
    deriving (Show, Read)

data FilterAttribute
    = Limit LimitAttribute Operator LimitValue
    | Action ActionAttribute ActionValue
    deriving (Show)

data Filter
    = Hide [FilterAttribute]
    | Show [FilterAttribute]
    deriving (Show)


defaultVolume = 50

numberP :: Parser Int
numberP = read <$> many1 digit

stringP :: Parser String
stringP = do
    s <- many1 (choice [letter, char ' '])
    return s

quotedStringP :: Parser String
quotedStringP = do
    char '"'
    s <- stringP
    char '"'
    return s

operatorP :: Parser Operator
operatorP =
        try (string "<=" >> return LessThanOrEqual)
    <|> try (string ">=" >> return GreaterThanOrEqual)
    <|> (string "<" >> return LessThan)
    <|> (string ">" >> return GreaterThan)
    <|> (string "=" >> return Equal)

colorP :: Parser Color
colorP =
    do {r <- numberP;
        char ' ';
        g <- numberP;
        char ' ';
        b <- numberP;
        optional $ char ' ';
        a <- optionMaybe numberP;
        return $ case a of
                    Nothing -> RGB r g b
                    (Just x) -> RGBA r g b x }

rarityP :: Parser LimitValue
rarityP = read <$> (choice $ map string ["Normal", "Magic", "Rare", "Unique"])

-- TODO: Limits can have multiple values
-- stepBy string(without spaces) or quotedString and space as spacer
limitP :: Parser FilterAttribute
limitP = do
    try (do {
        att <- read <$> (choice $ map string ["ItemLevel", "DropLevel", "Quality", "Sockets", "LinkedSockets", "Height", "Width"]);
        char ' ';
        op <- optionMaybe $ operatorP <* char ' ';
        v <- numberP;
        return $ case op of
            Nothing -> Limit att Equal $ Number v
            (Just x) -> Limit att x $ Number v })
    <|> do {
        att <- read <$> (choice $ map (try . string) ["Class", "BaseType", "SocketGroup"]);
        char ' ';
        s <- choice [stringP, quotedStringP];
        return $ Limit att Equal $ Text s; }
    <|> do {
        att <- string "Rarity";
        char ' ';
        op <- optionMaybe $ operatorP <* char ' ';
        r <- rarityP;
        return $ case op of
            Nothing -> Limit Rarity Equal r
            (Just x) -> Limit Rarity x r}

actionP :: Parser FilterAttribute
actionP = do
    try (do {
        string "SetFontSize";
        char ' ';
        d <- numberP;
        return $ Action SetFontSize (FontSize d) })
    <|> do {
        att <- colorAttributes;
        char ' ';
        color <- colorP;
        return $ Action att (Color color) }
    <|> do {
        string "PlayAlertSound";
        char ' ';
        id <- numberP; -- TODO: Shadows id function
        volume <- optionMaybe ( char ' ' >> numberP );
        return $ case volume of
            Nothing -> Action PlayAlertSound (Sound id defaultVolume)
            (Just x) -> Action PlayAlertSound (Sound id x) }
    where
        colorAttributes :: Parser ActionAttribute
        colorAttributes = read <$> (choice $ map (try . string) ["SetBorderColor", "SetTextColor", "SetBackgroundColor"])

filterP :: Parser Filter
filterP = do
    do {
        string "Hide";
        newline;
        ll <- optionMaybe $ many1 $ try (spaces *> limitP <* spaces);
        return $ case ll of
            Nothing  -> Hide []
            (Just x) -> Hide x}
    <|> do {
        string "Show";
        newline;
        ll <- many1 $ try $ (spaces *> choice [limitP, actionP] <* spaces);
        return $ Show ll}

parseInput :: String -> String
parseInput input = case parse (many1 (spaces *> filterP <* spaces) <* eof) "" input of
    Left err -> "No match: " ++ show err
    Right val -> show val
