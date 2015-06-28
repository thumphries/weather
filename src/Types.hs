{-# LANGUAGE DataKinds #-}
module Types where

import Data.Time (UTCTime)

data LogEntry = LogEntry
  { leTime    :: UTCTime
  , leMeasure :: Measurement
  } deriving (Show, Eq)

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

data Measurement
  = AU    (Location Kilometres) Celsius
  | US    (Location Miles)      Fahrenheit
  | FR    (Location Metres)     Kelvin
  | Other (Location Kilometres) Kelvin
  deriving (Show, Eq)

type DResolution = Double
type TResolution = Double



newtype Metres = Metres { unMetres :: DResolution }
  deriving (Show, Eq, Num, Fractional)

newtype Kilometres = Kilometres { unKilometres :: DResolution }
  deriving (Show, Eq, Num, Fractional)

kmToMetres :: Kilometres -> Metres
kmToMetres (Kilometres km) = Metres (km * 1000)

newtype Miles = Miles { unMiles :: DResolution }
  deriving (Show, Eq, Num, Fractional)

milesToMetres :: Miles -> Metres
milesToMetres (Miles m) = Metres (m * 1609.34)



newtype Kelvin = Kelvin { unKelvin :: TResolution }
  deriving (Show, Eq, Ord, Num, Fractional)

newtype Celsius = Celsius { unCelsius :: TResolution }
  deriving (Show, Eq, Ord, Num, Fractional)

celToKelvin :: Celsius -> Kelvin
celToKelvin (Celsius c) = Kelvin (c + 273.15)

newtype Fahrenheit = Fahrenheit { unFahrenheit :: TResolution }
  deriving (Show, Eq, Ord, Num, Fractional)

fahToKelvin :: Fahrenheit -> Kelvin
fahToKelvin (Fahrenheit f) = Kelvin ((f + 459.67) * (5/9))


{-
data Num a => Length a
  = Metres a
  | Kilometres a
  | Miles a
  deriving (Show, Eq)


-- problem: pretty ugly instance declaration here.
-- user will not expect the coercion to metres.
-- better to use newtypes

instance (Num a, Fractional a) => Num (Length a) where
  (Metres a) + (Metres b) = Metres (a + b)
  a + b = toMetres a + toMetres b
  (Metres a) * (Metres b) = Metres (a * b)
  a * b = toMetres a * toMetres b
  (Metres a) - (Metres b) = Metres (a - b)
  a - b = toMetres a - toMetres b
  abs (Metres a) = Metres (abs a)
  abs b = abs (toMetres b)
  signum (Metres a) = Metres (signum a)
  signum b = signum (toMetres b)
  fromInteger a = Metres (fromInteger a)

instance (Num a, Fractional a) => Fractional (Length a) where
  (Metres a) / (Metres b) = Metres (a / b)
  a / b = toMetres a / toMetres b
  fromRational a = Metres (fromRational a)

-}
