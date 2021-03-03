module Paracletus.Oblatum.Font where
-- functions for loading fonts
--import Prelude()
--import UPrelude
import FreeType
import Control.Monad ( when )
import Data.Char ( ord )
import Data.Word ( Word8 )
import Foreign.Marshal.Array ( peekArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( Storable(peek) )

data FontTex = FontTex Int Int [Word8]

data TTFData = TTFData
  { chIndex ∷ Int
  , chW     ∷ Double
  , chH     ∷ Double
  , chX     ∷ Double
  , chY     ∷ Double
  } deriving (Show, Eq)

loadFTChar ∷ FilePath → Char → Int → IO FontTex
loadFTChar fp char px = do
  ft_With_FreeType $ \lib →
    ft_With_Face lib fp 0 $ \face → do
      isScalable ← FT_IS_SCALABLE face
      when isScalable $ ft_Set_Char_Size face 0 ((fromIntegral px) * 64) 0 0
      ft_Load_Char face (fromIntegral $ ord char) FT_LOAD_RENDER
      slot ← peek . frGlyph =<< peek face
      withBitmap lib (gsrBitmap slot) $ \bmap → do
        let bufferSize = fromIntegral $ (bRows bmap) * fromIntegral (bPitch bmap)
        buffr ← peekArray bufferSize $ bBuffer bmap
        --drawBitmap (fromIntegral $ bPitch bmap) buffr
        return $ FontTex (fromIntegral (bPitch bmap)) (fromIntegral (bRows bmap)) buffr

withBitmap ∷ FT_Library → FT_Bitmap → (FT_Bitmap → IO a) → IO a
withBitmap lib source f =
  if any (== bPixel_mode source)
       [ FT_PIXEL_MODE_MONO, FT_PIXEL_MODE_GRAY2
       , FT_PIXEL_MODE_GRAY4, FT_PIXEL_MODE_BGRA ]
    then ft_Bitmap_With lib $ \targetPtr → do
           with source $ \sourcePtr → do
             ft_Bitmap_Convert lib sourcePtr targetPtr . fromIntegral $ bPixel_mode source
             f =<< peek targetPtr
    else f source

drawBitmap ∷ Int → [Word8] → IO ()
drawBitmap _ [] = return ()
drawBitmap n list = do
  putStrLn $ color <$> take n list
  drawBitmap n $ drop n list
  where
    color :: Word8 -> Char
    color a =
      case () of
        () | a == 0    -> ' '
           | a < 85    -> '░'
           | a < 170   -> '▒'
           | a < 255   -> '▓'
           | otherwise -> '█'

-- specific font data encoded here
chXUnit ∷ Double
chXUnit = 1.0 / 9.0
chYUnit ∷ Double
chYUnit = 1.0 / 7.0
indexTTF ∷ Char → TTFData
indexTTF '!' = TTFData 2   (1.0*chXUnit) (7.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF '"' = TTFData 3   (3.0*chXUnit) (3.0*chYUnit) (4.0*chXUnit)  (2.0*chYUnit)
indexTTF '#' = TTFData 4   (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '$' = TTFData 5   (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '%' = TTFData 6   (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '&' = TTFData 7   (7.0*chXUnit) (7.0*chYUnit) (8.0*chXUnit)  (0.0*chYUnit)
indexTTF '\'' = TTFData 8   (1.0*chXUnit) (2.0*chYUnit) (2.0*chXUnit)  (2.0*chYUnit)
indexTTF '(' = TTFData 9   (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF ')' = TTFData 10  (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF '*' = TTFData 11  (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (1.0*chYUnit)
indexTTF '+' = TTFData 12  (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (1.0*chYUnit)
indexTTF ',' = TTFData 13  (2.0*chXUnit) (3.0*chYUnit) (3.0*chXUnit)  (-2.0*chYUnit)
indexTTF '-' = TTFData 14  (5.0*chXUnit) (1.0*chYUnit) (6.0*chXUnit)  (1.0*chYUnit)
indexTTF '.' = TTFData 15  (1.0*chXUnit) (1.0*chYUnit) (2.0*chXUnit)  (-2.0*chYUnit)
indexTTF '/' = TTFData 16  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '0' = TTFData 17  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '1' = TTFData 18  (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF '2' = TTFData 19  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '3' = TTFData 20  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '4' = TTFData 21  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '5' = TTFData 22  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '6' = TTFData 23  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '7' = TTFData 24  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '8' = TTFData 25  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '9' = TTFData 26  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF ':' = TTFData 27  (1.0*chXUnit) (4.0*chYUnit) (2.0*chXUnit)  (0.5*chYUnit)
indexTTF ';' = TTFData 28  (1.0*chXUnit) (5.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF '<' = TTFData 29  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF '=' = TTFData 30  (5.0*chXUnit) (3.0*chYUnit) (6.0*chXUnit)  (0.5*chYUnit)
indexTTF '>' = TTFData 31  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF '?' = TTFData 32  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '@' = TTFData 33  (7.0*chXUnit) (7.0*chYUnit) (8.0*chXUnit)  (0.0*chYUnit)
indexTTF 'A' = TTFData 34  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'B' = TTFData 35  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'C' = TTFData 36  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'D' = TTFData 37  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'E' = TTFData 38  (4.0*chXUnit) (7.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'F' = TTFData 39  (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF 'G' = TTFData 40  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'H' = TTFData 41  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'I' = TTFData 42  (1.0*chXUnit) (7.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF 'J' = TTFData 43  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'K' = TTFData 44  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'L' = TTFData 45  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'M' = TTFData 46  (9.0*chXUnit) (7.0*chYUnit) (10.0*chXUnit) (0.0*chYUnit)
indexTTF 'N' = TTFData 47  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'O' = TTFData 48  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'P' = TTFData 49  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'Q' = TTFData 50  (6.0*chXUnit) (7.0*chYUnit) (7.0*chXUnit)  (0.0*chYUnit)
indexTTF 'R' = TTFData 51  (6.0*chXUnit) (7.0*chYUnit) (7.0*chXUnit)  (0.0*chYUnit)
indexTTF 'S' = TTFData 52  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'T' = TTFData 53  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'U' = TTFData 54  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'V' = TTFData 55  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'W' = TTFData 56  (9.0*chXUnit) (7.0*chYUnit) (10.0*chXUnit) (0.0*chYUnit)
indexTTF 'X' = TTFData 57  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'Y' = TTFData 58  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF 'Z' = TTFData 59  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF '[' = TTFData 60  (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF '\\' = TTFData 61  (5.0*chXUnit) (7.0*chYUnit) (6.0*chXUnit)  (0.0*chYUnit)
indexTTF ']' = TTFData 62  (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (0.0*chYUnit)
indexTTF '^' = TTFData 63  (5.0*chXUnit) (4.0*chYUnit) (6.0*chXUnit)  (2.0*chYUnit)
indexTTF '_' = TTFData 64  (7.0*chXUnit) (1.0*chYUnit) (8.0*chXUnit)  (0.0*chYUnit)
indexTTF '`' = TTFData 65  (2.0*chXUnit) (2.0*chYUnit) (3.0*chXUnit)  (2.0*chYUnit)
indexTTF 'a' = TTFData 66  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'b' = TTFData 67  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'c' = TTFData 68  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'd' = TTFData 69  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'e' = TTFData 70  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'f' = TTFData 71  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'g' = TTFData 72  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF 'h' = TTFData 73  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'i' = TTFData 74  (1.0*chXUnit) (6.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF 'j' = TTFData 75  (2.0*chXUnit) (7.0*chYUnit) (3.0*chXUnit)  (-2.0*chYUnit)
indexTTF 'k' = TTFData 76  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (0.0*chYUnit)
indexTTF 'l' = TTFData 77  (1.0*chXUnit) (6.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF 'm' = TTFData 78  (9.0*chXUnit) (5.0*chYUnit) (10.0*chXUnit) (-0.5*chYUnit)
indexTTF 'n' = TTFData 79  (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'o' = TTFData 80  (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'p' = TTFData 81  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF 'q' = TTFData 82  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF 'r' = TTFData 83  (3.0*chXUnit) (5.0*chYUnit) (4.0*chXUnit)  (-0.5*chYUnit)
indexTTF 's' = TTFData 84  (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF 't' = TTFData 85  (3.0*chXUnit) (6.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF 'u' = TTFData 86  (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'v' = TTFData 87  (5.0*chXUnit) (5.0*chYUnit) (6.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'w' = TTFData 88  (9.0*chXUnit) (5.0*chYUnit) (10.0*chXUnit) (-0.5*chYUnit)
indexTTF 'x' = TTFData 89  (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF 'y' = TTFData 90  (4.0*chXUnit) (6.0*chYUnit) (5.0*chXUnit)  (-2.0*chYUnit)
indexTTF 'z' = TTFData 91  (4.0*chXUnit) (5.0*chYUnit) (5.0*chXUnit)  (-0.5*chYUnit)
indexTTF '{' = TTFData 92  (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF '|' = TTFData 93  (1.0*chXUnit) (7.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
indexTTF '}' = TTFData 94  (3.0*chXUnit) (7.0*chYUnit) (4.0*chXUnit)  (0.0*chYUnit)
indexTTF '~' = TTFData 95  (5.0*chXUnit) (3.0*chYUnit) (6.0*chXUnit)  (1.0*chYUnit)
indexTTF _   = TTFData 0   (1.0*chXUnit) (1.0*chYUnit) (2.0*chXUnit)  (0.0*chYUnit)
