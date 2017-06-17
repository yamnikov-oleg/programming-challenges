module Main where

import           Data.Char          (ord)
import           Data.Word          (Word8)
import           System.Environment (getArgs)

type Username = String

-- RGB color with three 8-bit components.
data RGB = RGB Word8 Word8 Word8
  deriving (Show, Eq)

-- Angle in degrees
type Angle = Integer

-- Hue-Saturation-Lightness color.
data HSL = HSL Angle Double Double
  deriving (Show, Eq)

data Config = Config
  { configSaturation :: Double
  , configLightness  :: Double
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { configSaturation = 0.5
  , configLightness = 0.5
  }

black :: RGB
black = RGB 0 0 0

genHue :: Username -> Angle
genHue = (`mod` 360) . fromIntegral . foldl (\angle char -> angle*ord char + 1) 180

genHSL :: Config -> Username -> HSL
genHSL cfg name = HSL hue sat lig
  where
    hue = genHue name
    sat = configSaturation cfg
    lig = configLightness cfg

fitHsl :: HSL -> HSL
fitHsl (HSL h s l) = HSL (fitAngle h) (fitValue s) (fitValue l)
  where
    fitAngle a
      | a < 0 = fitAngle (a + 360)
      | a > 360 = fitAngle (a - 360)
      | otherwise = a
    fitValue v
      | v < 0 = 0
      | v > 1 = 1
      | otherwise = v

hslToRgb :: HSL -> RGB
hslToRgb hsl = RGB r g b
  where
    HSL h s l = fitHsl hsl
    chroma = (1 - abs (2*l - 1)) * s
    hPart = fromIntegral h / 60
    fracMod a b
      | a < b = a
      | otherwise = (a-b) `fracMod` b
    interm = chroma * (1 - abs ((hPart `fracMod` 2) - 1))
    (r_, g_, b_)
      | hPart <= 1 = (chroma, interm, 0)
      | hPart <= 2 = (interm, chroma, 0)
      | hPart <= 3 = (0, chroma, interm)
      | hPart <= 4 = (0, interm, chroma)
      | hPart <= 5 = (interm, 0, chroma)
      | hPart <= 6 = (chroma, 0, interm)
      | otherwise = error ("invalid hPart = " ++ show hPart)
    offset = l - 0.5 * chroma
    r = floor $ 256 * (r_ + offset)
    g = floor $ 256 * (g_ + offset)
    b = floor $ 256 * (b_ + offset)

genColor :: Config -> Username -> RGB
genColor cfg name = hslToRgb $ genHSL cfg name

main :: IO ()
main = do
  args <- getArgs
  let username = unwords args
  let config = defaultConfig
  print $ genColor config username
