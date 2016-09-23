{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
module Circuits.Analysis.DC where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Primitive.MutVar
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Generics.Deriving.Lens
import           GHC.Generics
import           GHC.Generics.Lens
import qualified Numeric.LinearAlgebra        as HMatrix
import qualified Numeric.LinearAlgebra.Data   as HMatrix

import           Circuits.Circuit

-- | Describes the time scale of a DC simulation. We can either perform a DC steady-state or a transient analysis.
data TimeScale
  = SteadyState
  -- ^ solving for steady state
  | Step
    { _currentTime :: Double -- ^ the current simulation time in seconds since t0
    , _timeStep    :: Double -- ^ the size of current time step to be taken
    }
    -- ^ solving for next time step

makeLenses ''TimeScale

-- | Type class providing an interface for the component stamps used in DC analysis.
class SimulateDC c where
  -- | Beginning to solve the next time step.
  beginSolve :: Simulation m Double sim => c -> TimeScale -> sim -> m ()
  default beginSolve :: (Generic c, GSimulateDC (Rep c), Simulation m Double sim) => c -> TimeScale -> sim -> m ()
  beginSolve c t s = forOf_ generic c $ \gc -> gbeginSolve gc t s
  -- | Beginning the next iteration of solving the non-linear parts.
  -- This should set up the right hand side based on the current element state.
  beginIteration :: Simulation m Double sim => c -> TimeScale -> sim -> m ()
  default beginIteration :: (Generic c, GSimulateDC (Rep c), Simulation m Double sim) => c -> TimeScale -> sim -> m ()
  beginIteration c t s = forOf_ generic c $ \gc -> gbeginIteration gc t s
  -- | End of the current iteration, after the referenced variables have been updated.
  -- It should update the state of non-linear parts, but not the state of linear parts.
  endIteration :: c -> TimeScale -> c
  default endIteration :: (Generic c, GSimulateDC (Rep c)) => c -> TimeScale -> c
  endIteration c t = over generic (`gendIteration` t) c
  -- | Finished solving the current time step. Here, the state of linear parts should be updated.
  endSolve :: c -> TimeScale -> c
  default endSolve :: (Generic c, GSimulateDC (Rep c)) => c -> TimeScale -> c
  endSolve c t = over generic (`gendSolve` t) c

instance SimulateDC Int where
  beginSolve _ _ _ = pure ()
  beginIteration _ _ _ = pure ()
  endIteration x _ = x
  endSolve x _ = x

instance SimulateDC Double where
  beginSolve _ _ _ = pure ()
  beginIteration _ _ _ = pure ()
  endIteration x _ = x
  endSolve x _ = x

instance SimulateDC Variable
instance SimulateDC Node
instance SimulateDC Branch

instance SimulateDC c => SimulateDC (Circuit c)

-- * Generic deriving of SimulateDC instances

class GSimulateDC f where
  gbeginSolve :: Simulation m Double sim => f p -> TimeScale -> sim -> m ()
  gbeginIteration :: Simulation m Double sim => f p -> TimeScale -> sim -> m ()
  gendIteration :: f p -> TimeScale -> f p
  gendSolve :: f p -> TimeScale -> f p

instance GSimulateDC V1 where
  gbeginSolve _ _ _ = return ()
  gbeginIteration _ _ _ = return ()
  gendIteration x _ = x
  gendSolve x _ = x

instance GSimulateDC U1 where
  gbeginSolve _ _ _ = return ()
  gbeginIteration _ _ _ = return ()
  gendIteration x _ = x
  gendSolve x _ = x

instance SimulateDC a => GSimulateDC (K1 i a) where
  gbeginSolve (K1 x) = beginSolve x
  gbeginIteration (K1 x) = beginIteration x
  gendIteration (K1 x) = K1 . endIteration x
  gendSolve (K1 x) = K1 . endSolve x

instance GSimulateDC f => GSimulateDC (M1 i t f) where
  gbeginSolve (M1 x) = gbeginSolve x
  gbeginIteration (M1 x) = gbeginIteration x
  gendIteration (M1 x) = M1 . gendIteration x
  gendSolve (M1 x) = M1 . gendSolve x

instance (GSimulateDC f, GSimulateDC g) => GSimulateDC (f :+: g) where
  gbeginSolve (L1 x) = gbeginSolve x
  gbeginSolve (R1 x) = gbeginSolve x
  gbeginIteration (L1 x) = gbeginIteration x
  gbeginIteration (R1 x) = gbeginIteration x
  gendIteration (L1 x) = L1 . gendIteration x
  gendIteration (R1 x) = R1 . gendIteration x
  gendSolve (L1 x) = L1 . gendSolve x
  gendSolve (R1 x) = R1 . gendSolve x

instance (GSimulateDC f, GSimulateDC g) => GSimulateDC (f :*: g) where
  gbeginSolve (x :*: y) t s = gbeginSolve x t s >> gbeginSolve y t s
  gbeginIteration (x :*: y) t s = gbeginIteration x t s >> gbeginIteration y t s
  gendIteration (x :*: y) t = gendIteration x t :*: gendIteration y t
  gendSolve (x :*: y) t = gendSolve x t :*: gendSolve y t

-- * DC Simulation

-- | A mutable system of linear equations suitable for representing linear circuits.
data SimulationState s v = SimulationState
  { size                 :: Int -- ^ the number of variables in this system
  , coefficients         :: VSM.MVector s v -- ^ the underlying storage of the equations coefficients in row-major order
  , coefficientsModified :: MutVar s Bool -- ^ flag indicating whether the coefficient matrix has been modified
  , rhs                  :: VSM.MVector s v -- ^ the right hand side of the equations
  , rhsModified          :: MutVar s Bool -- ^ flag indicating whether the RHS has been modified
  }

-- | Creates a new mutable linear circuit equation system with the given number of variables.
newSimulation :: (PrimMonad m, VSM.Storable v, Num v) => Int -> m (SimulationState (PrimState m) v)
newSimulation n =
  SimulationState n <$> VSM.replicate (n * n) 0 <*> newMutVar False <*> VSM.replicate n 0 <*> newMutVar False

cleanCoefficients :: (PrimMonad m) => SimulationState (PrimState m) v -> m ()
cleanCoefficients s = writeMutVar (coefficientsModified s) False

cleanRhs :: (PrimMonad m) => SimulationState (PrimState m) v -> m ()
cleanRhs s = writeMutVar (rhsModified s) False

-- | Computes the index into the flat buffer of matrix variables from a row and column.
flatIndex :: SimulationState s v -- ^ the equation system
          -> Int -- ^ matrix row
          -> Int -- ^ matrix column
          -> Int -- ^ flat buffer index
flatIndex sim row col = row * (size sim) + col

class Monad m => Simulation m v sys | sys -> v  where
  stampMatrix :: sys -> Variable -> Variable -> v -> m ()
  stampRhs :: sys -> Variable -> v -> m ()

instance (PrimMonad m, PrimState m ~ s, Num v, VS.Storable v) => Simulation m v (SimulationState s v) where
  stampMatrix sys (Index row _) (Index col _) value = do
    VSM.modify (coefficients sys) (+ value) (flatIndex sys row col)
    writeMutVar (coefficientsModified sys) True
  stampMatrix _ _ _ _ = return () -- no stamping when ground is involved

  stampRhs sys (Index row _) value = do
    VSM.modify (rhs sys) (+ value) row
    writeMutVar (rhsModified sys) True
  stampRhs _ Ground _ = return () -- no stamping for ground

initialSolution :: HasVariables c => Circuit c -> VS.Vector Double
initialSolution c = runST $ do
  vec <- VSM.new (c ^. variableCount)
  forOf_ (model . variables . _Index) c $ uncurry (VSM.write vec)
  VS.unsafeFreeze vec

step :: forall c. (HasVariables c, SimulateDC c) => TimeScale -> Circuit c -> Circuit c
step time c = runST simulate where
  simulate :: ST s (Circuit c)
  simulate = do
    sim <- newSimulation (c ^. variableCount)
    -- initialize matrix
    beginSolve c time sim
    mat <- HMatrix.reshape (c ^. variableCount) <$> VS.unsafeFreeze (coefficients sim)
    cleanCoefficients sim
    -- iterate
    writeMutVar (rhsModified sim) True
    c' <- solveNonLinear sim (HMatrix.luPacked mat) (initialSolution c) c
    -- finalize
    return $ endSolve c' time

  solveNonLinear :: SimulationState s Double -> HMatrix.LU Double -> VS.Vector Double -> Circuit c -> ST s (Circuit c)
  solveNonLinear sim lu prevSolution prevCircuit = do
    -- prepare RHS
    beginIteration c time sim
    rhsUpdated <- readMutVar (rhsModified sim)
    cleanRhs sim
    if rhsUpdated
      then do
        -- solve and update components
        rhs' <- VS.unsafeFreeze $ rhs sim
        let solution = HMatrix.flatten $ HMatrix.luSolve lu (HMatrix.asColumn rhs')
            c' = endIteration (updateValues solution c) time
            maxChange = VS.maximum $ VS.zipWith (\x y -> abs (x - y)) solution prevSolution
        if maxChange < 1e-6
          then return c'
          else solveNonLinear sim lu solution c'
      else return prevCircuit


-- | Update all variable references inside a circuit description with its new values.
updateValues :: HasVariables a => VS.Vector Double -> a -> a
updateValues values = over variables replace where
  replace Ground      = Ground
  replace (Index i _) = vars V.! i
  vars = V.imap Index $ VS.convert values
