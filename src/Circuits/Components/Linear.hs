{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
module Circuits.Components.Linear where

import           Control.Lens
import           Circuits.Circuit

-- | Describes a dependency on a voltage
data VoltageDependency = VoltageDependency
  { _voltageDependencyGain            :: Double -- ^ the conversion factor from the measured control voltage to the output voltage or current
  , _voltageDependencyNegativeControl :: Node -- ^ the voltage measuring negative node
  , _voltageDependencyPositiveControl :: Node -- ^ the voltage measuring positive node
  }
  deriving (Show)
makeFields ''VoltageDependency

-- | Describes a dependency on a current through a 0V voltage source
data CurrentDependency = CurrentDependency
  { _currentDependencyGain    :: Double -- ^ the conversion factor from the measured control current to the output voltage or current
  , _currentDependencyControl :: Branch -- ^ the 0V DC voltage source that is used for measuring the current
  }
  deriving (Show)
makeFields ''CurrentDependency

-- | Models independent and dependent source values.
data Source
  = Independent Double
  -- ^ independent source value
  | VoltageDependent VoltageDependency
  -- ^ source value is dependent on a voltage
  | CurrentDependent CurrentDependency
  -- ^ source value is dependent on a current
  deriving (Show)
makePrisms ''Source

-- | An ideal independent voltage source.
data VoltageSource = VoltageSource
  { _voltageSourceVoltage      :: Source -- ^ the voltage across the voltage source
  , _voltageSourceSelf         :: Branch -- ^ the id of the voltage source itself
  , _voltageSourceNegativeNode :: Node -- ^ the node the negative terminal is connected to
  , _voltageSourcePositiveNode :: Node -- ^ the node the positive terminal is connected to
  }
  deriving (Show)
makeFields ''VoltageSource

-- | An ideal independent current source.
data CurrentSource = CurrentSource
  { _currentSourceCurrent      :: Source -- ^ the current through the current source from the positive to the negative terminal
  , _currentSourceNegativeNode :: Node -- ^ the node the negative terminal is connected to
  , _currentSourcePositiveNode :: Node -- ^ the node the positive terminal is connected to
  }
  deriving (Show)
makeFields ''CurrentSource

-- | An ideal resistor.
data Resistor = Resistor
  { _resistorResistance   :: Double
  , _resistorNegativeNode :: Node -- ^ the node the negative terminal is connected to
  , _resistorPositiveNode :: Node -- ^ the node the positive terminal is connected to
  }
  deriving (Show)
makeFields ''Resistor
