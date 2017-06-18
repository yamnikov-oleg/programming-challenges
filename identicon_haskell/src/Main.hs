module Main where

import           Codec.Picture            (Image, PixelRGB8 (..), generateImage,
                                           writePng)
import           Data.Array               (Array, bounds, listArray, (!))
import           Data.Char                (ord)
import           Data.Either.Utils        (maybeToEither)
import           Data.Ix                  (range)
import           Data.List                (intercalate)
import           Data.String.Utils        (split)
import           Data.Word                (Word8)
import           System.Console.ArgParser (Descr (..), ParserSpec (..), andBy,
                                           optFlag, parsedBy, reqPos,
                                           withParseResult)
import           System.Environment       (getArgs)
import           Text.Read                (readMaybe)

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
  , configPadding    :: Integer
  , configOutputPath :: FilePath
  }
  deriving (Show, Eq)

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
    pad = configPadding cfg
    bmsize = bitMapSize bm
    imgdim = fromIntegral (bmsize * bitsize + pad*2)
    bg = configBackground cfg
    pixToBit x y
      | i < 0 = Nothing
      | j < 0 = Nothing
      | i >= bmsize = Nothing
      | j >= bmsize = Nothing
      | otherwise = Just $ bitMapInd bm (j, i)
      where
        i = (fromIntegral x - pad) `div` bitsize
        j = (fromIntegral y - pad) `div` bitsize
    bitToCol (Just True)  = rgb
    bitToCol (Just False) = bg
    bitToCol Nothing      = bg
    rgbToPixel (RGB r g b) = PixelRGB8 r g b

data CLI = CLI
  { cliUsername   :: String
  , cliSaturation :: Float
  , cliLightness  :: Float
  , cliBitmapSize :: Int
  , cliBitSize    :: Int
  , cliBackground :: String
  , cliPadding    :: Int
  , cliOutputPath :: FilePath
  }
  deriving (Show, Eq)

cliParser :: ParserSpec CLI
cliParser = CLI
  `parsedBy` reqPos "username"
  `andBy` optFlag 0.5 "s" `Descr` "Saturation of the foreground color (default - 0.5)"
  `andBy` optFlag 0.5 "l" `Descr` "Lightness of the foreground color (defailt - 0.5)"
  `andBy` optFlag 5 "bmsize" `Descr` "Bit map size in bits (default - 5)"
  `andBy` optFlag 32 "bitsize" `Descr` "Bit size in pixels (default - 32)"
  `andBy` optFlag "255,255,255" "bg" `Descr` "Background color (default - 255,255,255)"
  `andBy` optFlag 24 "pad" `Descr` "Padding in pixels (default - 24)"
  `andBy` optFlag "out.png" "o" `Descr` "Output file path (default - out.png)"

cliToConfig :: CLI -> Either String (Username, Config)
cliToConfig cli = do
  let username = cliUsername cli
  bg <- maybeToEither "could not parse background color" $ parseColor $ cliBackground cli
  let config = Config
        { configSaturation = realToFrac $ cliSaturation cli
        , configLightness = realToFrac $ cliLightness cli
        , configBitmapSize = fromIntegral $ cliBitmapSize cli
        , configBitSize = fromIntegral $ cliBitSize cli
        , configBackground = bg
        , configPadding = fromIntegral $ cliPadding cli
        , configOutputPath = cliOutputPath cli
        }
  Right (username, config)
  where
    parseInts = mapM readMaybe . split ","
    intsToColor [r, g, b] = Just $ RGB r g b
    intsToColor _         = Nothing
    parseColor str = do
      ints <- parseInts str
      intsToColor ints

runApp' :: Config -> Username -> IO()
runApp' cfg name = do
  let color = genColor cfg name
  putStrLn ("Generated color: " ++ show color)
  let bitmap = genBitMap cfg name
  putStrLn "Generated bit map:"
  print bitmap
  putStrLn ("Writing to '" ++ configOutputPath cfg ++ "'...")
  writePng (configOutputPath cfg) (drawImage cfg color bitmap)

runApp :: CLI -> IO ()
runApp cli =
  case cliToConfig cli of
    Left msg          -> putStrLn msg
    Right (name, cfg) -> runApp' cfg name

main :: IO ()
main = withParseResult cliParser runApp
