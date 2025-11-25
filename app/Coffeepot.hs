module Coffeepot where

data MilkType
  = Cream | HalfAndHalf | WholeMilk | PartSkim | Skim | NonDairy
  deriving (Eq)

instance Show MilkType where
  show Cream       = "Cream"
  show HalfAndHalf = "Half-and-half"
  show WholeMilk   = "Whole-milk"
  show PartSkim    = "Part-Skim"
  show Skim        = "Skim"
  show NonDairy    = "Non-Dairy"

data SyrupType
  = Vanilla | Almond | Raspberry | Chocolate
  deriving (Show, Eq)

data AlcoholType
  = Whisky | Rum | Kahlua | Aquavit
  deriving (Show, Eq)

data SweetenerType
  = Sugar | Honey | Stevia | Artificial
  deriving (Show, Eq)

data SpiceType
  = Cinnamon | Nutmeg | Clove | Cardamom
  deriving (Show, Eq)

data AdditionType
    = AddAll -- "*"
    | AddMilk MilkType
    | AddSyrup SyrupType
    | AddSweetener SweetenerType
    | AddSpice SpiceType
    | AddAlcohol AlcoholType
    deriving (Show, Eq)

data Category
  = Milk | Syrup | Sweetener | Spice | Alcohol
  deriving (Show, Eq)

data AdditionCapability
  = AllowAll
  | AllowCategory Category
  | AllowSpecific AdditionType
  deriving (Show, Eq)

data State
  = Off
  | Idle
  | Brewing
  deriving (Show, Eq)

data Coffeepot = Coffeepot
  { state       :: State
  , temperature :: !Double
  , waterLevel  :: !Int
  , coffeeLevel :: !Int
  , supported   :: [AdditionCapability]
  } deriving (Eq)

instance Show Coffeepot where
  show pot = "state=" ++ show (state pot) ++ 
             ",temperature=" ++ show (temperature pot) ++
             ",waterLevel=" ++ show (waterLevel pot) ++
             ",coffeeLevel=" ++ show (coffeeLevel pot) ++
             ",supported=" ++ show (supported pot)

additionCategory :: AdditionType -> Category
additionCategory additionType =
  case additionType of
    AddAll         -> error "AddAll has no single category"
    AddMilk _      -> Milk
    AddSyrup _     -> Syrup
    AddSweetener _ -> Sweetener
    AddSpice _     -> Spice
    AddAlcohol _   -> Alcohol

supports :: Coffeepot -> AdditionType -> Bool
supports coffeepot additionType =
    AllowAll `elem` capabilities
  || AllowSpecific additionType `elem` capabilities
  || AllowCategory (additionCategory additionType) `elem` capabilities
  where
    capabilities = supported coffeepot

addSupport :: AdditionCapability -> Coffeepot -> Coffeepot
addSupport capability coffeepot =
    if capability `elem` supported coffeepot
        then coffeepot
        else coffeepot {
            supported = capability : supported coffeepot
        }

removeSupport :: AdditionCapability -> Coffeepot -> Coffeepot
removeSupport capability coffeepot =
    coffeepot {
        supported = filter (/= capability) (supported coffeepot)
    }

allowMilk :: MilkType -> AdditionCapability
allowMilk = AllowSpecific . AddMilk

allowSyrup :: SyrupType -> AdditionCapability
allowSyrup = AllowSpecific . AddSyrup

allowCategory :: Category -> AdditionCapability
allowCategory = AllowCategory

data CoffeepotError
    = UnsupportedAddition AdditionType
    | ActionNotAllowedInState State
    | AlreadyInState
    | InsufficientWater Int
    | InsufficientCoffee Int
  deriving (Show, Eq)

type CoffeepotResult = Either CoffeepotError Coffeepot

powerOn :: Coffeepot -> CoffeepotResult
powerOn coffeepot =
  case state coffeepot of
    Off   -> Right coffeepot { state = Idle }
    _     -> Left AlreadyInState

powerOff :: Coffeepot -> CoffeepotResult
powerOff coffeepot =
  case state coffeepot of
    Off     -> Left AlreadyInState
    Brewing -> Left (ActionNotAllowedInState Brewing)
    Idle    -> Right coffeepot { state = Off }


coffeePerCup :: Int
coffeePerCup = 10

checkAdditionsSupported :: Coffeepot -> [AdditionType] -> Either CoffeepotError ()
checkAdditionsSupported pot additions =
  case filter (not . supports pot) additions of
    []                      -> Right ()
    (unsupported : _)       -> Left (UnsupportedAddition unsupported)

calculateTotalWater :: [AdditionType] -> Int
calculateTotalWater additions =
  waterPerCup + (length additions * waterPerAddition)
  where
    waterPerCup = 100
    waterPerAddition = 20

validateResources :: Coffeepot -> Either CoffeepotError Coffeepot
validateResources pot
  | lackWater > 0   = Left (InsufficientWater lackWater)
  | lackCoffee > 0  = Left (InsufficientCoffee lackCoffee)
  | otherwise       = Right pot
  where
    currentWater = waterLevel pot
    currentCoffee = coffeeLevel pot
    lackWater = coffeePerCup - currentWater
    lackCoffee = coffeePerCup - currentCoffee

brewStart :: Coffeepot -> [AdditionType] -> Either CoffeepotError Coffeepot
brewStart coffeepot additions =
  case state coffeepot of
    Off     -> Left (ActionNotAllowedInState Off)
    Brewing -> Left AlreadyInState
    Idle    -> do
      checkAdditionsSupported coffeepot additions
      let totalWater = calculateTotalWater additions
      let updatedPot = coffeepot
            { state = Brewing
            , waterLevel = waterLevel coffeepot - totalWater
            , coffeeLevel = coffeeLevel coffeepot - coffeePerCup
            }
      validateResources updatedPot

brewStop :: Coffeepot -> CoffeepotResult
brewStop coffeepot =
  case state coffeepot of
    Brewing -> Right coffeepot { state = Idle }
    currentState -> Left (ActionNotAllowedInState currentState)
