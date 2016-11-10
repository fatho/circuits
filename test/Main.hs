{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Text.Printf

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

data RC = RC
  { _rcGnd :: Node
  , _rcN1  :: Node
  , _rcN2  :: Node
  , _rcVS  :: VoltageSource
  , _rcR   :: Resistor
  , _rcC   :: Capacitor
  }
  deriving (Show, Generic, SimulateDC, HasVariables)
makeLenses ''RC

rc :: Circuit RC
rc = buildCircuit $ do
  gnd <- ground
  n1 <- newNode
  n2 <- newNode
  vs <- newBranch
  return RC
    { _rcGnd = gnd
    , _rcN1  = n1
    , _rcN2  = n2
    , _rcVS  = VoltageSource (Independent 5) vs gnd n2
    , _rcR   = Resistor 100 gnd n1
    , _rcC   = Capacitor 0.01 n1 n2
    }

main :: IO ()
main = do
  let c2 = step SteadyState c1
  print c2
  let rcs = transient (Transient 0 4 0.1) rc
  forMOf_ (traverse . Control.Lens.to (over _2 $ view $ model . rcC . voltageAcross)) rcs $ uncurry $ printf "%.1f ~ %.2f\n"
  --mapM_ (\(t, c) -> printf "t = %.1f\n%s\n\n" t (show c)) rcs
  return ()


data Transient = Transient
  { _transientStartTime :: Double
  , _transientStopTime  :: Double
  , _transientTimeStep  :: Double
  }

transient :: (SimulateDC c, HasVariables c) => Transient -> Circuit c -> [(Double, Circuit c)]
transient (Transient start stop dt) c0 = go c0 start where
  go c t
    | t > stop = []
    | otherwise = (t, c) : go (step (Step t dt) c) (t + dt)

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

