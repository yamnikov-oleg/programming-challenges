module Main where

import           Codec.Picture      (Image, PixelRGB8 (..), generateImage,
                                     writePng)
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
  show bm =
    intercalate "\n" $
    map (\i ->
      map (\j ->
        bitToChar $ bitMapInd bm (i, j)
      ) $ range (0, size-1)
    ) $
    range (0, size-1)
    where
      size = bitMapSize bm
      bitToChar True  = 'X'
      bitToChar False = ' '

bitMapSizeToArraySize :: Integer -> (Integer, Integer)
bitMapSizeToArraySize size = (halfWidth, height)
  where
    height = size
    halfWidth = if even size
      then size `div` 2
      else ((size - 1) `div` 2) + 1

bitMapFromList :: Integer -> [Bool] -> SimBitMap
bitMapFromList size = SimBitMap . listArray ((0, 0), (height-1, halfWidth-1))
  where
    (halfWidth, height) = bitMapSizeToArraySize size

bitMapSize :: SimBitMap -> Integer
bitMapSize (SimBitMap arr) = maxi - mini + 1
  where
    ((mini, _), (maxi, _)) = bounds arr

bitMapInd :: SimBitMap -> (Integer, Integer) -> Bool
bitMapInd bm@(SimBitMap arr) (i, j) = arr ! (i, j_)
  where
    size = bitMapSize bm
    (hwid, _) = bitMapSizeToArraySize size
    j_ = if j >= hwid
      then size - j - 1
      else j

data Config = Config
  { configSaturation :: Double
  , configLightness  :: Double
  , configBitmapSize :: Integer
  , configBitSize    :: Integer
  , configBackground :: RGB
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { configSaturation = 0.5
  , configLightness = 0.5
  , configBitmapSize = 5
  , configBitSize = 32
  , configBackground = RGB 255 255 255
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

bitMapFromNum :: Integer -> Integer -> SimBitMap
bitMapFromNum size num =
  bitMapFromList size $ bitseq (halfWidth * height) num
  where
    (halfWidth, height) = bitMapSizeToArraySize size

genBitMap :: Config -> Username -> SimBitMap
genBitMap cfg = bitMapFromNum size . genBounded (2^(halfWidth*height))
  where
    size = configBitmapSize cfg
    (halfWidth, height) = bitMapSizeToArraySize size

drawImage :: Config -> RGB -> SimBitMap -> Image PixelRGB8
drawImage cfg rgb bm = generateImage (\x y -> rgbToPixel $ bitToCol $ pixToBit x y) imgdim imgdim
  where
    bitsize = configBitSize cfg
    bmsize = bitMapSize bm
    imgdim = fromIntegral (bmsize * bitsize)
    bg = configBackground cfg
    pixToBit x y = Just $ bitMapInd bm (fromIntegral y `div` bitsize, fromIntegral x `div` bitsize)
    bitToCol (Just True)  = rgb
    bitToCol (Just False) = bg
    bitToCol Nothing      = bg
    rgbToPixel (RGB r g b) = PixelRGB8 r g b

main :: IO ()
main = do
  args <- getArgs
  let username = unwords args
  let config = defaultConfig
  let rgb = genColor config username
  let bm = genBitMap config username
  let img = drawImage config rgb bm
  print rgb
  print bm
  writePng "out.png" img
