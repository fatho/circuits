{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
module Circuits.Linear where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as VSM

-- | Describes a dependency on a voltage
data VoltageDependency b n v = VoltageDependency
  { _voltageDependencyGain            :: v -- ^ the conversion factor from the measured control voltage to the output voltage or current
  , _voltageDependencyNegativeControl :: n -- ^ the voltage measuring negative node
  , _voltageDependencyPositiveControl :: n -- ^ the voltage measuring positive node
  }
makeFields ''VoltageDependency

-- | Describes a dependency on a current through a 0V voltage source
data CurrentDependency b n v = CurrentDependency
  { _currentDependencyGain      :: v -- ^ the conversion factor from the measured control current to the output voltage or current
  , _currentDependencyControl :: b -- ^ the 0V DC voltage source that is used for measuring the current
  }
makeFields ''CurrentDependency

-- | Models independent and dependent source values.
data Source b n v
  = Independent v
  -- ^ independent source value
  | VoltageDependent (VoltageDependency b n v)
  -- ^ source value is dependent on a voltage
  | CurrentDependent (CurrentDependency b n v)
  -- ^ source value is dependent on a current
makePrisms ''Source

-- | An ideal independent voltage source.
data VoltageSource b n v = VoltageSource
  { _voltageSourceVoltage      :: Source b n v -- ^ the voltage across the voltage source
  , _voltageSourceSelf         :: b -- ^ the id of the voltage source itself
  , _voltageSourceNegativeNode :: n -- ^ the node the negative terminal is connected to
  , _voltageSourcePositiveNode :: n -- ^ the node the positive terminal is connected to
  }
makeFields ''VoltageSource

-- | An ideal independent current source.
data CurrentSource b n v = CurrentSource
  { _currentSourceCurrent      :: Source b n v -- ^ the current through the current source from the positive to the negative terminal
  , _currentSourceNegativeNode :: n -- ^ the node the negative terminal is connected to
  , _currentSourcePositiveNode :: n -- ^ the node the positive terminal is connected to
  }
makeFields ''CurrentSource

-- | An ideal resistor.
data Resistor b n v = Resistor
  { _resistorResistance   :: v
  , _resistorNegativeNode :: n -- ^ the node the negative terminal is connected to
  , _resistorPositiveNode :: n -- ^ the node the positive terminal is connected to
  }
makeFields ''Resistor

-- | A mutable system of linear equations suitable for representing linear circuits.
data MSystem s v = MSystem
  { _msystemNodes   :: Int -- ^ the number of nodes in this system
  , _msystemSources :: Int -- ^ the number of voltage sources in this system
  , _msystemCoeff   :: VSM.MVector s v -- ^ the underlying storage of the equations coefficients in row-major order
  , _msystemRhs     :: VSM.MVector s v -- ^ the right hand side of the equations
  }

-- | Creates a new mutable linear circuit equation system with the given number of nodes and voltage sources.
newMSystem :: (PrimMonad m, VSM.Storable v, Num v) => Int -> Int -> m (MSystem (PrimState m) v)
newMSystem numNodes numSources =
  MSystem numNodes numSources <$> VSM.replicate (n * n) 0 <*> VSM.replicate n 0
  where n = numNodes + numSources

-- | A reference to a node in the system of equations.
data NodeRef = Ground | Var Int
makePrisms ''NodeRef

-- | Computes the index into the flat buffer of matrix elements from a row and column.
flatIndex :: MSystem s v -- ^ the equation system
          -> Int -- ^ matrix row
          -> Int -- ^ matrix column
          -> Int -- ^ flat buffer index
flatIndex MSystem{..} row col = row * (_msystemNodes + _msystemSources) + col

stampMatrix :: (PrimMonad m, Num v, VSM.Storable v) => MSystem (PrimState m) v -> NodeRef -> NodeRef -> v -> m ()
stampMatrix sys@MSystem{..} (Var row) (Var col) value = VSM.modify _msystemCoeff (+ value) (flatIndex sys row col)
stampMatrix _ _ _ _ = return () -- no stamping when ground is involved

stampRhs :: (PrimMonad m, Num v, VSM.Storable v) => MSystem (PrimState m) v -> NodeRef -> v -> m ()
stampRhs MSystem{..} (Var row) value = VSM.modify _msystemRhs (+ value) row
stampRhs _ Ground _ = return () -- no stamping for ground

class Stamp c where
  stamp :: (PrimMonad m, Fractional v, VSM.Storable v) => MSystem (PrimState m) v -> c v -> m ()

instance Stamp (VoltageSource NodeRef NodeRef) where
  stamp sys@MSystem{..} vs = do
    forM_ (zip [1, -1] [vs ^. positiveNode, vs ^. negativeNode]) $ \(sign, node) -> do
      stampMatrix sys (vs ^. self) node sign
      stampMatrix sys node (vs ^. self) sign
    case vs ^. voltage of
      Independent v -> stampRhs sys (vs ^. self) v
      VoltageDependent vd -> do
        stampMatrix sys (vs ^. self) (vd ^. negativeControl) (vd ^. gain)
        stampMatrix sys (vs ^. self) (vd ^. positiveControl) (negate $ vd ^. gain)
      CurrentDependent cd ->
        stampMatrix sys (vs ^. self) (cd ^. control) (negate $ cd ^. gain)

instance Stamp (CurrentSource NodeRef NodeRef) where
  stamp sys cs = case cs ^. current of
    Independent c -> do
      stampRhs sys (cs ^. positiveNode) (- c)
      stampRhs sys (cs ^. negativeNode) c
    VoltageDependent vd -> do
      stampMatrix sys (cs ^. positiveNode) (vd ^. negativeControl) (negate $ vd ^. gain)
      stampMatrix sys (cs ^. positiveNode) (vd ^. positiveControl) (vd ^. gain)
      stampMatrix sys (cs ^. negativeNode) (vd ^. negativeControl) (vd ^. gain)
      stampMatrix sys (cs ^. negativeNode) (vd ^. positiveControl) (negate $ vd ^. gain)
    CurrentDependent cd -> do
      stampMatrix sys (cs ^. positiveNode) (cd ^. control) (cd ^. gain)
      stampMatrix sys (cs ^. negativeNode) (cd ^. control) (negate $ cd ^. gain)

instance Stamp (Resistor NodeRef NodeRef) where
  stamp sys r =  do
    let conductance = recip $ r ^. resistance
    stampMatrix sys (r ^. positiveNode) (r ^. positiveNode) conductance
    stampMatrix sys (r ^. negativeNode) (r ^. negativeNode) conductance
    stampMatrix sys (r ^. positiveNode) (r ^. negativeNode) (-conductance)
    stampMatrix sys (r ^. negativeNode) (r ^. positiveNode) (-conductance)
