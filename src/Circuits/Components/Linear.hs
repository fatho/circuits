{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
module Circuits.Components.Linear where

import Control.Monad
import           Control.Lens
import GHC.Generics

import           Circuits.Circuit
import Circuits.Analysis.DC

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
    stampMatrix sys (r ^. positiveNode . var) (r ^. positiveNode . var) conductance
    stampMatrix sys (r ^. negativeNode . var) (r ^. negativeNode . var) conductance
    stampMatrix sys (r ^. positiveNode . var) (r ^. negativeNode . var) (-conductance)
    stampMatrix sys (r ^. negativeNode . var) (r ^. positiveNode . var) (-conductance)
  beginIteration _ _ _ = return ()
  endIteration x _ = x
  endSolve x _ = x
