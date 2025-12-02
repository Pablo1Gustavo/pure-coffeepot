{-# LANGUAGE OverloadedStrings #-}

module Server where

import Coffeepot
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.Wai
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)

data SafeNature
    = SafeYes
    | SafeNo
    | ConditionalSafe Text
    deriving (Eq)

instance Show SafeNature where
    show SafeYes = "yes"
    show SafeNo = "no"
    show (ConditionalSafe condition) = "if-" ++ T.unpack condition

safeHeader :: SafeNature -> Header
safeHeader safeNature =
    ("Safe", TE.encodeUtf8 . T.pack $ show safeNature)

coffeepotErrorToResponse :: CoffeepotError -> (Status, Text)
coffeepotErrorToResponse err = case err of
    UnsupportedAddition additionType ->
        ( notAcceptable406
        , T.pack $ "Unsupported addition: " ++ show additionType
        )
    ActionNotAllowedInState state ->
        ( conflict409
        , T.pack $ "Action not allowed in state: " ++ show state
        )
    AlreadyInState ->
        ( conflict409
        , "Already in the requested state"
        )
    InsufficientWater amount ->
        ( serviceUnavailable503
        , T.pack $ "Insufficient water: need " ++ show amount ++ " ml more"
        )
    InsufficientCoffee amount ->
        ( serviceUnavailable503
        , T.pack $ "Insufficient coffee: need " ++ show amount ++ " g more"
        )

errorResponse :: CoffeepotError -> Response
errorResponse err = 
    responseLBS errorStatus [safeHeader SafeYes] (LBS.fromStrict . TE.encodeUtf8 $ errorMessage)
    where (errorStatus, errorMessage) = coffeepotErrorToResponse err

parseAdditionType :: Text -> Maybe AdditionType
parseAdditionType additionText =
    case T.toLower (T.strip additionText) of
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
        -- Alcohol types
        "whisky"       -> Just (AddAlcohol Whisky)
        "rum"          -> Just (AddAlcohol Rum)
        "kahlua"       -> Just (AddAlcohol Kahlua)
        "aquavit"      -> Just (AddAlcohol Aquavit)
        -- Unrecognized
        _              -> Nothing

parseAdditionItem :: Text -> Maybe AdditionType
parseAdditionItem item =
    let withoutParams = T.takeWhile (/= ';') item
        normalized = T.strip withoutParams
    in parseAdditionType normalized

splitByComma :: Text -> [Text]
splitByComma textValue = 
    map T.strip (T.splitOn "," textValue)

parseAcceptAdditions :: Text -> [AdditionType]
parseAcceptAdditions headerValue =
    if AddAll `elem` additionList then [AddAll] else additionList
    where
        items = splitByComma headerValue
        parsed = map parseAdditionItem items
        additionList = catMaybes parsed

isCoffeepotContentType :: Maybe Text -> Bool
isCoffeepotContentType = (== Just "message/coffeepot")

checkCoffeepotContentType :: Request -> Either Response ()
checkCoffeepotContentType req =
    case lookup "Content-Type" (requestHeaders req) of
        Nothing -> Left $ responseLBS unsupportedMediaType415 [] "Missing Content-Type header"
        Just contentType 
            | contentType == "message/coffeepot" -> Right ()
            | otherwise -> Left $ responseLBS unsupportedMediaType415 [] "Unsupported Media Type: Expected Content-Type: message/coffeepot"
