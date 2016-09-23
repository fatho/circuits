{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell #-}
module Circuits.Analysis.DC where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as VSM

data TimeScale
  = SteadyState
  -- ^ solving for steady state
  | Step
    { _currentTime :: Double -- ^ the current simulation time in seconds since t0
    , _timeStep :: Double -- ^ the size of current time step to be taken
    }
    -- ^ solving for next time step
makeLenses ''TimeScale

-- | Type class providing an interface for the component stamps used in DC analysis.
class StampDC c where
  dcStampInitial   :: c -> TimeScale -> m ()
  dcStampIteration :: c -> TimeScale -> m ()

