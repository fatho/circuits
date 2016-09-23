{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE RecursiveDo    #-}
module Main where

--import           Circuits.Network
import           Control.Lens
import           Control.Monad.Fix
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           GHC.Generics
import qualified Numeric.LinearAlgebra        as HMatrix
import qualified Numeric.LinearAlgebra.Data   as HMatrix

import           Circuits.Analysis.DC
import           Circuits.Circuit
import           Circuits.Components.Linear

data TestCircuit = TestCircuit
  { _gnd :: Node
  , _n1  :: Node
  , _vs  :: VoltageSource
  , _r   :: Resistor
  }
  deriving (Show, Generic, SimulateDC, HasVariables)

c1 :: Circuit TestCircuit
c1 = buildCircuit $ do
  gnd <- ground
  n1 <- newNode
  vs <- newBranch
  return TestCircuit
    { _gnd = gnd
    , _n1  = n1
    , _vs  = VoltageSource (Independent 5) vs gnd n1
    , _r   = Resistor 100 gnd n1
    }

main :: IO ()
main = do
  let c2 = step SteadyState c1
  print c2

  {-
  sys <- newMSystem 1 1
  stamp sys (VoltageSource (Independent 5) (Var 1) Ground (Var 0))
  stamp sys (Resistor 100 Ground (Var 0) :: Resistor NodeRef NodeRef Double)
  stamp sys (Resistor 200 Ground (Var 0) :: Resistor NodeRef NodeRef Double)
  mv <- HMatrix.reshape 2 <$> VS.unsafeFreeze (_msystemCoeff sys)
  rhs <- HMatrix.asColumn <$> VS.unsafeFreeze (_msystemRhs sys)
  print mv
  print rhs
  print $ HMatrix.linearSolve mv rhs
-}
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

