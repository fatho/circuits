{-# LANGUAGE ExistentialQuantification #-}
module Main where

--import           Circuits.Network
import Circuits.Circuit
import Circuits.Components.Linear
import           Control.Lens
import Control.Monad.Fix
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM

import qualified Numeric.LinearAlgebra as HMatrix
import qualified Numeric.LinearAlgebra.Data as HMatrix

data TestCircuit = TestCircuit
  { gnd :: Node
  , n1  :: Node
  , vs  :: VoltageSource
  , r   :: Resistor
  }
  deriving (Show)

-- data AComponent = forall c. Component c => AComponent c

class Circuit c where
  nodes    :: Traversal' c Node
  branches :: Traversal' c Branch

instance Circuit TestCircuit where
  nodes f c = TestCircuit <$> f (gnd c) <*> f (n1 c) <*> pure (vs c) <*> pure (r c)
  branches f c = TestCircuit <$> pure (gnd c) <*> pure (n1 c) <*> self f (vs c) <*> pure (r c)

c1 :: TestCircuit
c1 = buildCircuit $ mfix $ \c -> TestCircuit
  <$> ground
  <*> newNode
  <*> fmap (\self -> VoltageSource (Independent 5) self (gnd c) (n1 c)) newBranch
  <*> pure (Resistor 100 (gnd c) (n1 c))

main :: IO ()
main = do
  print c1
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

