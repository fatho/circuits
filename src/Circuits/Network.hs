{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Circuits.Network where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.ST
import qualified Data.Array                   as A
import qualified Data.Array.MArray            as AM
import qualified Data.Array.ST                as AST
import           Data.Foldable
import qualified Data.Ix                      as Ix
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Set.Lens
import qualified Data.Vector                  as V
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Numeric.LinearAlgebra        as HMatrix

import qualified Circuits.Internal.Util       as Util

type Volt    = Double
type Ampere  = Double
type Ohm     = Double
type Coulomb = Double
type Farad   = Double

data Component n
  = VoltageSource { _voltage :: Volt, _negativeNode :: n, _positiveNode :: n }
  | CurrentSource { _current :: Ampere, _negativeNode :: n, _positiveNode :: n }
  | Resistor { _resistance :: Ohm, _node1 :: n, _node2 :: n }
  | Capacitor { _capacity :: Farad, _negativeNode :: n, _positiveNode :: n }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

makePrisms ''Component
makeLenses ''Component

data Circuit n p = Circuit
  { _circuitParts  :: Map p (Component n)
  , _circuitGround :: n
  }
  deriving (Eq, Ord, Show, Read)

makeLenses ''Circuit

-- | Lens to an individual part of the circuit.
part :: Ord p => p -> Lens' (Circuit n p) (Maybe (Component n))
part p = circuitParts . at p

-- | The empty circuit network, with only a ground node.
empty :: n -> Circuit n p
empty gnd = Circuit Map.empty gnd

-- | Type of variables in the linear system resulting from the modified nodal analysis.
data CircuitVar n p
  = NodeVoltage n
  -- ^ Variable for the voltage at node @n@.
  | SourceCurrent p
  -- ^ Variable for the current through voltage source @p@ from positive to negative terminal.
  deriving (Eq, Ord, Show, Read)
makePrisms ''CircuitVar

data CompiledCircuit n p = CompiledCircuit
  { _compiledParts    :: !(Map p (Component (Maybe Int, n)))
  , _compiledVars     :: !(V.Vector (CircuitVar n p))
  , _compiledCoeff    :: !(HMatrix.Matrix Double)
  , _compiledRhs      :: !(HMatrix.Vector Double)
  , _compiledLU       :: !(HMatrix.LU Double)
  , _compiledSolution :: !(HMatrix.Matrix Double)
  }
  deriving (Show)
makeLenses ''CompiledCircuit

variableIndex :: (Ord p, Ord n) => CircuitVar n p -> CompiledCircuit n p -> Maybe Int
variableIndex v cc = Util.binarySearch v (cc ^. compiledVars)

compile :: (Ord n, Ord p) => Circuit n p -> Either String (CompiledCircuit n p)
compile circuit = do
    unless (Set.member ground nodeSet)
      $ throwError "Ground not connected"
    let coeffHM = HMatrix.fromArray2D coeffMatrix
    return CompiledCircuit
      { _compiledParts = partsIxRefs
      , _compiledVars = variables
      , _compiledCoeff = coeffHM
      , _compiledRhs   = rhsVector
      , _compiledLU    = HMatrix.luPacked coeffHM
      , _compiledSolution = HMatrix.konst 0 (numVars, 1)
      }
  where
    ground = circuit ^. circuitGround
    -- find and index all nodes and voltage sources
    nodeSet = setOf (circuitParts . folded . folded) circuit
    activeNodeSet = Set.delete ground nodeSet
    nodeVars = map NodeVoltage $ Set.toAscList activeNodeSet
    vsVars = map (SourceCurrent . fst) $ filter (has $ _2 . failing _VoltageSource _Capacitor) $ Map.toAscList $ circuit ^. circuitParts
    variables = V.fromList $ nodeVars ++ vsVars
    numVars = V.length variables

    -- add indices to components
    partsIxRefs = over (mapped . mapped) (\n -> (Util.binarySearch (NodeVoltage n) variables, n)) (circuit ^. circuitParts)

    -- building coefficient matrix for linear equations
    pairs xs = (,) <$> xs <*> xs

    modifyArray arr idx f = do
      old <- AM.readArray arr idx
      AM.writeArray arr idx (f old)

    asSTArray :: ST s (AST.STArray s i e) -> ST s (AST.STArray s i e)
    asSTArray = id

    (coeffMatrix, rhsVector) = runST $ do
      coeffs <- asSTArray $ AM.newArray ((0, 0), (numVars - 1, numVars - 1)) 0
      rhs    <- VSM.replicate numVars 0

      iforM_ partsIxRefs $ \partId part -> case part of
        Resistor{..} -> do
          let conductance = recip _resistance
              nodeIx = mapMaybe fst [_node1, _node2] -- find non-ground nodes
          forM_ (pairs nodeIx) $ \(row, col) -> do
            let sign = if row == col then 1 else -1
            modifyArray coeffs (row, col) (+ conductance * sign)
        CurrentSource{..} -> do
          forM_ (fst _positiveNode) $ \idx -> VSM.modify rhs (subtract _current) idx
          forM_ (fst _negativeNode) $ \idx -> VSM.modify rhs (+ _current) idx
        vs@VoltageSource{} -> writeVoltageSource coeffs rhs partId vs
        Capacitor{..} -> writeVoltageSource coeffs rhs partId (VoltageSource 0 _negativeNode _positiveNode)

      (,) <$> AM.freeze coeffs <*> VS.freeze rhs

    writeVoltageSource coeffs rhs partId VoltageSource{..} = do
      let sourceIx = fromJust $ Util.binarySearch (SourceCurrent partId) variables
          terminals = mapMaybe sequence $ zip [1, -1] $ map fst [_positiveNode, _negativeNode]
      VSM.write rhs sourceIx _voltage
      forM_ terminals $ \(sign, term) -> do
        AM.writeArray coeffs (sourceIx, term) sign
        AM.writeArray coeffs (term, sourceIx) sign

solve :: CompiledCircuit n p -> HMatrix.Matrix Double
solve cc = HMatrix.luSolve (cc ^. compiledLU) (HMatrix.asColumn $ cc ^. compiledRhs)

step :: (Ord n, Ord p) => Double -> CompiledCircuit n p -> CompiledCircuit n p
step dt cc = cc { _compiledRhs = newRHS, _compiledSolution = newSolution } where
  -- 1. step capacitors
  newRHS = VS.modify updateRHS (cc ^. compiledRhs)
  updateRHS rhs =
    iforOf_ (compiledParts . itraversed . _Capacitor) cc $ \partId (capacity, negNode, posNode) -> do
      let capIndex = fromJust $ variableIndex (SourceCurrent partId) cc
          capCurrent = HMatrix.atIndex (cc ^. compiledSolution) (capIndex, 0)
          dvolt = capCurrent * dt / capacity
      VSM.modify rhs (+ dvolt) capIndex
  -- 2. compute new solution
  newSolution = solve cc { _compiledRhs = newRHS }
  -- 3. step inductors
