module Main where

--import           Circuits.Network
import Circuits.Linear
import           Control.Lens
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM

import qualified Numeric.LinearAlgebra as HMatrix
import qualified Numeric.LinearAlgebra.Data as HMatrix

main :: IO ()
main = do
  sys <- newMSystem 1 1
  stamp sys (VoltageSource (Independent 5) (Var 1) Ground (Var 0))
  stamp sys (Resistor 100 Ground (Var 0) :: Resistor NodeRef NodeRef Double)
  stamp sys (Resistor 200 Ground (Var 0) :: Resistor NodeRef NodeRef Double)
  mv <- HMatrix.reshape 2 <$> VS.unsafeFreeze (_msystemCoeff sys)
  rhs <- HMatrix.asColumn <$> VS.unsafeFreeze (_msystemRhs sys)
  print mv
  print rhs
  print $ HMatrix.linearSolve mv rhs
{-  let simple = empty "GND"
        & part "R1" .~ Just (Resistor 100 "N1" "GND")
        & part "C1" .~ Just (Capacitor 0.01 "N2" "N1")
        & part "Vs" .~ Just (VoltageSource 5 "GND" "N2")

  case compile simple of
    Left err -> putStrLn err
    Right c -> do
      let cs = take 100 $ iterate (step 0.01) c
      mapM_ print cs
-}

