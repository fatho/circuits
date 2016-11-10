{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
module Circuits.Components.Linear where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Function
import           Debug.Trace
import           GHC.Generics

import           Circuits.Analysis.DC
import           Circuits.Circuit

-- * Components

-- | Describes a dependency on a voltage
data VoltageDependency = VoltageDependency
  { _voltageDependencyGain            :: Double -- ^ the conversion factor from the measured control voltage to the output voltage or current
  , _voltageDependencyNegativeControl :: Node -- ^ the voltage measuring negative node
  , _voltageDependencyPositiveControl :: Node -- ^ the voltage measuring positive node
  }
  deriving (Show, Generic, HasVariables)
makeFields ''VoltageDependency

-- | Describes a dependency on a current through a 0V voltage source
data CurrentDependency = CurrentDependency
  { _currentDependencyGain    :: Double -- ^ the conversion factor from the measured control current to the output voltage or current
  , _currentDependencyControl :: Branch -- ^ the 0V DC voltage source that is used for measuring the current
  }
  deriving (Show, Generic, HasVariables)
makeFields ''CurrentDependency

-- | Models independent and dependent source values.
data Source
  = Independent Double
  -- ^ independent source value
  | VoltageDependent VoltageDependency
  -- ^ source value is dependent on a voltage
  | CurrentDependent CurrentDependency
  -- ^ source value is dependent on a current
  deriving (Show, Generic, HasVariables)
makePrisms ''Source

-- | An ideal independent voltage source.
data VoltageSource = VoltageSource
  { _voltageSourceVoltage      :: Source -- ^ the voltage across the voltage source
  , _voltageSourceSelf         :: Branch -- ^ the id of the voltage source itself
  , _voltageSourceNegativeNode :: Node -- ^ the node the negative terminal is connected to
  , _voltageSourcePositiveNode :: Node -- ^ the node the positive terminal is connected to
  }
  deriving (Show, Generic, HasVariables)
makeFields ''VoltageSource

instance SimulateDC VoltageSource where
  beginSolve vs _ sys = do
    forM_ (zip [1, -1] [vs ^. positiveNode . var, vs ^. negativeNode . var]) $ \(sign, node) -> do
      stampMatrix sys (vs ^. self . var) node sign
      stampMatrix sys node (vs ^. self . var) sign
    case vs ^. voltage of
      Independent v -> stampRhs sys (vs ^. self . var) v
      VoltageDependent vd -> do
        stampMatrix sys (vs ^. self . var) (vd ^. negativeControl . var) (vd ^. gain)
        stampMatrix sys (vs ^. self . var) (vd ^. positiveControl . var) (negate $ vd ^. gain)
      CurrentDependent cd ->
        stampMatrix sys (vs ^. self . var) (cd ^. control . var) (negate $ cd ^. gain)
  beginIteration _ _ _ = return ()
  endIteration x _ = x
  endSolve x _ = x

-- | An ideal independent current source.
data CurrentSource = CurrentSource
  { _currentSourceCurrent      :: Source -- ^ the current through the current source from the positive to the negative terminal
  , _currentSourceNegativeNode :: Node -- ^ the node the negative terminal is connected to
  , _currentSourcePositiveNode :: Node -- ^ the node the positive terminal is connected to
  }
  deriving (Show, Generic, HasVariables)
makeFields ''CurrentSource

instance SimulateDC CurrentSource where
  beginSolve cs _ sys = case cs ^. current of
    Independent c -> do
      stampRhs sys (cs ^. positiveNode . var) (- c)
      stampRhs sys (cs ^. negativeNode . var) c
    VoltageDependent vd -> do
      stampMatrix sys (cs ^. positiveNode . var) (vd ^. negativeControl . var) (negate $ vd ^. gain)
      stampMatrix sys (cs ^. positiveNode . var) (vd ^. positiveControl . var) (vd ^. gain)
      stampMatrix sys (cs ^. negativeNode . var) (vd ^. negativeControl . var) (vd ^. gain)
      stampMatrix sys (cs ^. negativeNode . var) (vd ^. positiveControl . var) (negate $ vd ^. gain)
    CurrentDependent cd -> do
      stampMatrix sys (cs ^. positiveNode . var) (cd ^. control . var) (cd ^. gain)
      stampMatrix sys (cs ^. negativeNode . var) (cd ^. control . var) (negate $ cd ^. gain)
  beginIteration _ _ _ = return ()
  endIteration x _ = x
  endSolve x _ = x

-- | An ideal resistor.
data Resistor = Resistor
  { _resistorResistance   :: Double
  , _resistorNegativeNode :: Node -- ^ the node the negative terminal is connected to
  , _resistorPositiveNode :: Node -- ^ the node the positive terminal is connected to
  }
  deriving (Show, Generic, HasVariables)
makeFields ''Resistor

instance SimulateDC Resistor where
  beginSolve r _ sys = do
    let conductance = recip $ r ^. resistance
    stampResistor sys (r ^. positiveNode) (r ^. negativeNode) conductance
  beginIteration _ _ _ = return ()
  endIteration x _ = x
  endSolve x _ = x

-- | Stamps a resistor onto a circuit equation matrix.
stampResistor :: (Simulation m v sys, Num v)
              => sys -> Node -> Node -> v -> m ()
stampResistor sys pos neg conductance = do
  stampMatrix sys (pos ^. var) (pos ^. var) conductance
  stampMatrix sys (neg ^. var) (neg ^. var) conductance
  stampMatrix sys (pos ^. var) (neg ^. var) (-conductance)
  stampMatrix sys (neg ^. var) (pos ^. var) (-conductance)

-- | Norton-equivalent of capacitor
data Capacitor = Capacitor
  { _capacitorCapacity     :: Double -- ^ the capacitance of the capacitor (measured in F)
  , _capacitorNegativeNode :: Node
  , _capacitorPositiveNode :: Node
  }
  deriving (Show, Generic, HasVariables)
makeFields ''Capacitor

instance SimulateDC Capacitor where
  -- in the DC steady state, a capacitor behaves as if there is no connection at all
  beginSolve _ SteadyState _ = return ()
  beginSolve cap (Step _ dt) sys = do
    -- REMARK: currently just Backward Euler is implemented
    -- conductance of resistor in Norton equivalent
    let g = view capacity cap / dt
    -- current of current source in Norton equivalent
    let i = view capacity cap / dt * view voltageAcross cap
    stampResistor sys (cap ^. positiveNode) (cap ^. negativeNode) g
    -- current source is running opposite
    stampRhs sys (cap ^. positiveNode . var) i
    stampRhs sys (cap ^. negativeNode . var) (- i)

  -- capacitor behaves like a linear component in each step, no iterations necessary
  beginIteration _ _ _ = return ()
  endIteration cap _ = cap

  endSolve cap _ = cap

-- * Utility Functions

voltageAcross :: (HasPositiveNode c Node, HasNegativeNode c Node) => Getter c Double
voltageAcross = Control.Lens.to $
  liftA2 ((-) `on` view nodeVoltage) (view positiveNode) (view negativeNode)
