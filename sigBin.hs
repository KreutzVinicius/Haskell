data SigBin = Neg Bin | Pos Bin deriving (Eq)

data Bin = V | Z Bin | U Bin deriving (Eq)

instance Show Bin where
  show V = "0"
  show (Z bin) = show bin ++ "0"
  show (U bin) = show bin ++ "1"

instance Show SigBin where
  show (Neg bin) = "-" ++ show bin
  show (Pos bin) = "+" ++ show bin

binToInteger :: Bin -> Integer
binToInteger V = 0
binToInteger (Z bin) = 2 * binToInteger bin
binToInteger (U bin) = 2 * binToInteger bin + 1

sigBinToInteger :: SigBin -> Integer
sigBinToInteger (Neg bin) = - (binToInteger bin)
sigBinToInteger (Pos bin) = binToInteger bin

integerToBin :: Integer -> Bin
integerToBin 0 = V
integerToBin n = if even n then Z (integerToBin (n `div` 2)) else U (integerToBin (n `div` 2))

integerToSigBin :: Integer -> SigBin
integerToSigBin n = if n < 0 then Neg (integerToBin (- n)) else Pos (integerToBin n)

instance Num SigBin where
  (+) a b = integerToSigBin (sigBinToInteger a + sigBinToInteger b)
  (*) a b = integerToSigBin (sigBinToInteger a * sigBinToInteger b)
  (-) a b = integerToSigBin (sigBinToInteger a - sigBinToInteger b)
  abs a = if sigBinToInteger a < 0 then Neg (integerToBin (abs (sigBinToInteger a))) else Pos (integerToBin (abs (sigBinToInteger a)))
  signum a = integerToSigBin (signum (sigBinToInteger a))
  fromInteger = integerToSigBin

main :: IO ()
main = do
  let a = integerToSigBin 18
  let b = integerToSigBin (-5)
  print (sigBinToInteger a)
  print (sigBinToInteger b)
  print (sigBinToInteger (a + b))
  print (sigBinToInteger (a * b))
  print (sigBinToInteger (a - b))
  print (sigBinToInteger (abs a))
  print (sigBinToInteger (abs b))
  print (sigBinToInteger (signum a))
  print (sigBinToInteger (signum b))