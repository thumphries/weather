module Types where

import Data.Time (UTCTime)

data LogEntry = LogEntry
  { leTime    :: UTCTime
  , leMeasure :: Measurement
  } deriving (Show)

data Station = AUS | FRA | USA | Other String
  deriving (Show, Eq)

-- alternative: Length and Temperature typeclasses
-- ... and use a GADT for measurement
-- What do we even get out of this?
-- ... 1. Unique 'station' representation, questionably useful
-- ... 2. Distributes the station logic to the parser? Needs to be
--        enforced somewhere... ok, here
-- ... 3. Lose some ugly named functions like celToKel
-- ... 4. Can normalise the data without erasing the station. Good!

-- XXX Show constraint is regrettable. Luckily we coerce to Integer
-- when rendering, so it will go away.
data Measurement where
  Measurement :: (Length a, Temperature b, Show a, Show b)
              => Station -> Location a -> b -> Measurement

deriving instance Show Measurement

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

data Fractional a => Location a = Location
  { locX :: a
  , locY :: a
  } deriving (Show, Eq)

type DUnit = Double
type TUnit = Double


class Fractional a => Length a where
  toMetres :: a -> Metres

class Fractional a => Temperature a where
  toKelvin :: a -> Kelvin


newtype Metres = Metres { unMetres :: DUnit }
  deriving (Show, Eq, Num, Fractional)

instance Length Metres where
  toMetres = id


newtype Kilometres = Kilometres { unKilometres :: DUnit }
  deriving (Show, Eq, Num, Fractional)

instance Length Kilometres where
  toMetres (Kilometres km) = Metres (km * 1000)



newtype Miles = Miles { unMiles :: DUnit }
  deriving (Show, Eq, Num, Fractional)

-- XXX Check conversion
instance Length Miles where
  toMetres (Miles m) = Metres (m * 1609.34)



newtype Kelvin = Kelvin { unKelvin :: TUnit }
  deriving (Show, Eq, Ord, Num, Fractional)

instance Temperature Kelvin where
  toKelvin = id



newtype Celsius = Celsius { unCelsius :: TUnit }
  deriving (Show, Eq, Ord, Num, Fractional)

instance Temperature Celsius where
  toKelvin (Celsius c) = Kelvin (c + 273.15)



newtype Fahrenheit = Fahrenheit { unFahrenheit :: TUnit }
  deriving (Show, Eq, Ord, Num, Fractional)

-- XXX Check conversion
instance Temperature Fahrenheit where
  toKelvin (Fahrenheit f) = Kelvin ((f + 459.67) * (5/9))
