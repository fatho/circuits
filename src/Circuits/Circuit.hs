{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
module Circuits.Circuit where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Primitive
import           Control.Monad.State.Class
import qualified Data.Vector.Storable.Mutable as VSM
import           Generics.Deriving.Lens
import           GHC.Generics
import           GHC.Generics.Lens

-- | Opaque reference to a variable in the equation system.
data Variable = Ground | Index { _elementIndex :: Int, _elementValue :: Double }
  deriving (Show, Generic)
makePrisms ''Variable
makeLenses ''Variable

newtype Node = Node { _nodeVar :: Variable }
  deriving (Show, Generic)
makeFields ''Node

newtype Branch = Branch { _branchVar :: Variable }
  deriving (Show, Generic)
makeFields ''Branch

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

newVariable :: Double -> CircuitBuilder Variable
newVariable initial = Index <$> (nextId <<+= 1) <*> pure initial

newBranch :: CircuitBuilder Branch
newBranch = Branch <$> newVariable 0

newNode :: CircuitBuilder Node
newNode = Node <$> newVariable 0

ground :: CircuitBuilder Node
ground = Node <$> pure Ground

buildCircuit :: CircuitBuilder a -> Circuit a
buildCircuit builder = Circuit model (_nextId st) where
  (model, st) = runCircuitBuilder builder (BuilderState 0)

data Circuit descr = Circuit
  { _model         :: descr -- ^ the model describing the circuit
  , _variableCount :: Int -- ^ the number of variables in the circuit
  } deriving (Show, Generic, HasVariables)

-- * Traversal of variables in a circuit description

class HasVariables c where
  variables :: Traversal' c Variable

  default variables :: (Generic c, GHasVariables (Rep c)) => Traversal' c Variable
  variables = generic . gvariables

instance HasVariables Node where
  variables = var

instance HasVariables Branch where
  variables = var

instance HasVariables Double where
  variables = ignored
instance HasVariables Int where
  variables = ignored

class GHasVariables g where
  gvariables :: Traversal' (g p) Variable

instance GHasVariables V1 where
  gvariables = ignored

instance GHasVariables U1 where
  gvariables = ignored

instance (GHasVariables l, GHasVariables r) => GHasVariables (l :+: r) where
  gvariables f (L1 l) = L1 <$> gvariables f l
  gvariables f (R1 r) = R1 <$> gvariables f r

instance (GHasVariables l, GHasVariables r) => GHasVariables (l :*: r) where
  gvariables f (l :*: r) = (:*:) <$> gvariables f l <*> gvariables f r

instance (HasVariables c) => GHasVariables (K1 i c) where
  gvariables = _K1 . variables

instance GHasVariables g => GHasVariables (M1 i t g) where
  gvariables = _M1 . gvariables

-- * Lenses
makeLenses ''Circuit

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

-- | Computes the index into the flat buffer of matrix variables from a row and column.
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
