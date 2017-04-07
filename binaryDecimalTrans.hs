import Data.Char (digitToInt)

binaryToDecimal :: String -> String
binaryToDecimal str =
  show $ foldl (\acc x -> acc*2 + x) 0 $ map digitToInt str

decimalToBinary :: String -> String
decimalToBinary str =
  concat $ map show $ reverse $ help $ foldl (\acc x -> acc*10 + x) 0 $ map digitToInt str
  where
    help 0 = []
    help n = (mod n 2) : (help $ div n 2)
