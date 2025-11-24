{-# LANGUAGE OverloadedStrings #-}

import Coffeepot
import Web.Scotty
import Network.HTTP.Types.Status
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Control.Monad (unless)

coffeepotErrorToResponse :: CoffeepotError -> (Status, Text)
coffeepotErrorToResponse err = case err of
    UnsupportedAddition additionType ->
        ( badRequest400
        , TL.pack $ "Unsupported addition: " ++ show additionType
        )
    ActionNotAllowedInState state ->
        ( conflict409
        , TL.pack $ "Action not allowed in state: " ++ show state
        )
    AlreadyInState ->
        ( conflict409
        , "Already in the requested state"
        )
    InsufficientWater amount ->
        ( serviceUnavailable503
        , TL.pack $ "Insufficient water: need " ++ show amount ++ " ml more"
        )
    InsufficientCoffee amount ->
        ( serviceUnavailable503
        , TL.pack $ "Insufficient coffee: need " ++ show amount ++ " g more"
        )


parseAcceptAdditions :: Text -> Maybe [AdditionType]
parseAcceptAdditions headerValue =
    let items = splitByComma headerValue
        parsed = map parseAdditionItem items
        maybeList = sequence parsed
    in case maybeList of
        Nothing -> Nothing
        Just additionList -> 
            if AddAll `elem` additionList
            then Just [AddAll]
            else Just additionList

splitByComma :: Text -> [Text]
splitByComma textValue = 
    map TL.strip (TL.splitOn "," textValue)

parseAdditionItem :: Text -> Maybe AdditionType
parseAdditionItem item =
    let withoutParams = TL.takeWhile (/= ';') item
        normalized = TL.strip withoutParams
    in parseAdditionType normalized

parseAdditionType :: Text -> Maybe AdditionType
parseAdditionType additionText =
    case TL.toLower additionText of
        "*"            -> Just AddAll
        -- Milk types
        "cream"        -> Just (AddMilk Cream)
        "half-and-half" -> Just (AddMilk HalfAndHalf)
        "whole-milk"   -> Just (AddMilk WholeMilk)
        "part-skim"    -> Just (AddMilk PartSkim)
        "skim"         -> Just (AddMilk Skim)
        "non-dairy"    -> Just (AddMilk NonDairy)
        -- Syrup types
        "vanilla"      -> Just (AddSyrup Vanilla)
        "almond"       -> Just (AddSyrup Almond)
        "raspberry"    -> Just (AddSyrup Raspberry)
        "chocolate"    -> Just (AddSyrup Chocolate)
        -- Sweetener types
        "sugar"        -> Just (AddSweetener Sugar)
        "honey"        -> Just (AddSweetener Honey)
        "stevia"       -> Just (AddSweetener Stevia)
        "artificial"   -> Just (AddSweetener Artificial)
        -- Spice types
        "cinnamon"     -> Just (AddSpice Cinnamon)
        "nutmeg"       -> Just (AddSpice Nutmeg)
        "clove"        -> Just (AddSpice Clove)
        "cardamom"     -> Just (AddSpice Cardamom)
        -- Alcohol types
        "whisky"       -> Just (AddAlcohol Whisky)
        "rum"          -> Just (AddAlcohol Rum)
        "kahlua"       -> Just (AddAlcohol Kahlua)
        "aquavit"      -> Just (AddAlcohol Aquavit)
        -- Unrecognized
        _              -> Nothing

isCoffeepotContentType :: Maybe Text -> Bool
isCoffeepotContentType = (== Just "message/coffeepot")

requireCoffeepotContentTypePure :: ActionM ()
requireCoffeepotContentTypePure = do
    contentType <- header "Content-Type"
    unless (isCoffeepotContentType contentType) $ do
        status unsupportedMediaType415
        text "Unsupported Media Type: Expected Content-Type: message/coffeepot"

data SafeNature
    = SafeYes
    | SafeNo
    | ConditionalSafe Text
    deriving (Show, Eq)

parseSafeHeader :: Text -> Maybe SafeNature
parseSafeHeader headerValue
    | normalized == "yes" = Just SafeYes
    | normalized == "no" = Just SafeNo
    | TL.isPrefixOf "if-" normalized = Just (ConditionalSafe (TL.drop 3 normalized))
    | otherwise = Nothing
  where
    normalized = TL.toLower (TL.strip headerValue)

