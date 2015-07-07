module Types where

import Data.Time (UTCTime)

data LogEntry = LogEntry
  { leTime    :: !UTCTime
  , leMeasure :: !Measurement
  }

data Station = AUS | FRA | USA | Other String
  deriving (Eq)

instance Show Station where
  show s = case s of
    AUS -> "AU"
    FRA -> "FR"
    USA -> "US"
    Other st -> take 2 st

data Measurement = forall a b. (Length a, Temperature b)
                 => Measurement Station (Location a) b

-- Station units are enforced here, and here only.
mkMeasure :: Station -> (DUnit, DUnit) -> TUnit -> Measurement
mkMeasure c@AUS (lx, ly) t =
  Measurement c (Location (Kilometres lx) (Kilometres ly)) (Celsius t)
mkMeasure c@FRA (lx, ly) t =
  Measurement c (Location (Metres lx)     (Metres ly))     (Kelvin t)
mkMeasure c@USA (lx, ly) t =
  Measurement c (Location (Miles lx)      (Miles ly))      (Fahrenheit t)
mkMeasure c@(Other s) (lx, ly) t =
  Measurement c (Location (Metres lx)     (Metres ly))     (Kelvin t)

-- Converting between units in Haskell is a bit of a shitshow.
-- Someone with more time to spend on this exercise would force one of
-- these libs to work:
-- - NumericPrelude supports all the units we need, but is a yak nest
-- - `dimensional` and `units` both choose to ignore Fahrenheit
-- - `quantities` is full of strings
-- - `dimensional-dk` handles everything nicely but isn't on Hackage yet

data RealFrac a => Location a = Location
  { locX :: a
  , locY :: a
  } deriving (Show, Eq)

type DUnit = Double
type TUnit = Double

-- XXX This is only used for dispatching / monomorphising.
-- Should be able to replace it with GHC extension magic, DK, PK
data DistUnit = KM | MI | ME deriving (Show)
data TempUnit = C | K | F    deriving (Show)

class RealFrac a => Length a where
  toMetres :: a -> Metres
  fromMetres :: Metres -> a
  lconvert :: Length b => b -> a
  lconvert = fromMetres . toMetres

class RealFrac a => Temperature a where
  toKelvin :: a -> Kelvin
  fromKelvin :: Kelvin -> a
  tconvert :: Temperature b => b -> a
  tconvert = fromKelvin . toKelvin


-- Lengths
newtype Metres = Metres { unMetres :: DUnit }
  deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac)

instance Length Metres where
  toMetres = id
  fromMetres = id

newtype Kilometres = Kilometres { unKilometres :: DUnit }
  deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac)

instance Length Kilometres where
  toMetres (Kilometres km) = Metres (km * 1000)
  fromMetres (Metres m) = Kilometres (m / 1000)

newtype Miles = Miles { unMiles :: DUnit }
  deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac)

instance Length Miles where
  toMetres (Miles m) = Metres (m * 1609.34)
  fromMetres (Metres m) = Miles (m / 1609.34)


-- Temperatures
newtype Kelvin = Kelvin { unKelvin :: TUnit }
  deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac)

instance Temperature Kelvin where
  toKelvin = id
  fromKelvin = id

newtype Celsius = Celsius { unCelsius :: TUnit }
  deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac)

instance Temperature Celsius where
  toKelvin (Celsius c) = Kelvin (c + 273.15)
  fromKelvin (Kelvin k) = Celsius (k - 273.15)

newtype Fahrenheit = Fahrenheit { unFahrenheit :: TUnit }
  deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac)

instance Temperature Fahrenheit where
  toKelvin (Fahrenheit f) = Kelvin ((f + 459.67) * (5/9))
  fromKelvin (Kelvin k) = Fahrenheit ((k * (9/5)) - 459.67)
