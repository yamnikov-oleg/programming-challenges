module Main where

import           Data.Array         (Array, bounds, listArray, (!))
import           Data.Char          (ord)
import           Data.Ix            (range)
import           Data.List          (intercalate)
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

-- Simetric bit map.
--
-- If it contains elements:
-- [ [ True, False ]
-- , [ True, False ]
-- , [ False, True ]
-- ]
--
-- then it is displayed that way (mirrored by right edge):
-- "X X"
-- "X X"
-- " X "
newtype SimBitMap = SimBitMap (Array (Integer, Integer) Bool)
  deriving (Eq)

instance Show SimBitMap where
  show (SimBitMap arr) =
    intercalate "\n" $
    map (mirror [] . showRow arr (minj, maxj)) $
    range (mini, maxi)
    where
      ((mini, minj), (maxi, maxj)) = bounds arr
      mirror acc []     = []
      mirror acc [x]    = x:acc
      mirror acc (x:xs) = x : mirror (x:acc) xs
      showRow arr (minj, maxj) i =
        concatMap (\ind -> if arr ! ind then "X" else " ") $
        range ((i, minj), (i, maxj))

data Config = Config
  { configSaturation      :: Double
  , configLightness       :: Double
  , configBitmapHalfWidth :: Integer
  , configBitmapHeight    :: Integer
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { configSaturation = 0.5
  , configLightness = 0.5
  , configBitmapHalfWidth = 3
  , configBitmapHeight = 5
  }

genBounded :: Integer -> Username -> Integer
genBounded bound =
  (`mod` bound) .
  fromIntegral .
  foldl (\angle char -> angle * fromIntegral (ord char) + 1) (bound `div` 2)

genHue :: Username -> Angle
genHue = genBounded 360

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

bitseq :: Integer -> Integer -> [Bool]
bitseq 0 _     = []
bitseq len num = (num `mod` 2 == 1) : bitseq (len-1) (num `div` 2)

bitMapFromNum :: (Integer, Integer) -> Integer -> SimBitMap
bitMapFromNum (hwid, hei) num =
  SimBitMap $ listArray ((1, 1), (hei, hwid)) $ bitseq (hwid * hei) num

genBitMap :: Config -> Username -> SimBitMap
genBitMap cfg = bitMapFromNum (hwid, hei) . genBounded (2^(hwid*hei))
  where
    hwid = configBitmapHalfWidth cfg
    hei = configBitmapHeight cfg

main :: IO ()
main = do
  args <- getArgs
  let username = unwords args
  let config = defaultConfig
  print $ genColor config username
  print $ genBitMap config username
