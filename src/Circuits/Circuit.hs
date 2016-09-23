{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Circuits.Circuit where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Class
import           Control.Monad.Primitive
import qualified Data.Vector.Storable.Mutable as VSM

data ElementType = Node' | Branch'

-- | Opaque reference to a variable in the equation system.
data Element (t :: ElementType) = Ground | Index Int
  deriving (Show)
makePrisms ''Element

type Node = Element 'Node'
type Branch = Element 'Branch'

data BuilderState = BuilderState
  { _nextId  :: Int
  }
makeLenses ''BuilderState

newtype CircuitBuilder a = CircuitBuilder { runCircuitBuilder :: BuilderState -> (a, BuilderState) }

instance Functor CircuitBuilder where
  fmap f b = CircuitBuilder $ \s -> over _1 f $ runCircuitBuilder b s

instance Applicative CircuitBuilder where
  pure x = CircuitBuilder $ \s -> (x, s)
  cf <*> ca = CircuitBuilder $ \s -> let ~(f, s') = runCircuitBuilder cf s
                                     in over _1 f $ runCircuitBuilder ca s'

instance Monad CircuitBuilder where
  cb >>= f = CircuitBuilder $ \s ->
    let ~(x, s') = runCircuitBuilder cb s
    in runCircuitBuilder (f x) s'

instance MonadFix CircuitBuilder where
  mfix f = CircuitBuilder $ \s ->
    let ~(r, s') = runCircuitBuilder (f r) s
    in (r, s')

instance MonadState BuilderState CircuitBuilder where
  get = CircuitBuilder $ \s -> (s, s)
  put s = CircuitBuilder $ const ((), s)

newVariable :: CircuitBuilder (Element t)
newVariable = Index <$> (nextId <<+= 1)

newBranch :: CircuitBuilder Branch
newBranch = newVariable

newNode :: CircuitBuilder Node
newNode = newVariable

ground :: CircuitBuilder Node
ground = pure Ground

buildCircuit :: CircuitBuilder a -> a
buildCircuit builder = fst $ runCircuitBuilder builder (BuilderState 0)

{-
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

-- | A reference to a node or voltage source in the system of equations.
data NodeRef = Ground | Var Int
makePrisms ''NodeRef

-- | Computes the index into the flat buffer of matrix elements from a row and column.
flatIndex :: MSystem s v -- ^ the equation system
          -> Int -- ^ matrix row
          -> Int -- ^ matrix column
          -> Int -- ^ flat buffer index
flatIndex MSystem{..} row col = row * (_msystemNodes + _msystemSources) + col

-- | Stamps an entry in the matrix, ignoring the ground node.
stampMatrix :: (PrimMonad m, Num v, VSM.Storable v) => MSystem (PrimState m) v -> NodeRef -> NodeRef -> v -> m ()
stampMatrix sys@MSystem{..} (Var row) (Var col) value = VSM.modify _msystemCoeff (+ value) (flatIndex sys row col)
stampMatrix _ _ _ _ = return () -- no stamping when ground is involved

-- | Stamps an entry in the right hand side of the equations, ignoring the ground node.
stampRhs :: (PrimMonad m, Num v, VSM.Storable v) => MSystem (PrimState m) v -> NodeRef -> v -> m ()
stampRhs MSystem{..} (Var row) value = VSM.modify _msystemRhs (+ value) row
stampRhs _ Ground _                  = return () -- no stamping for ground
-}
