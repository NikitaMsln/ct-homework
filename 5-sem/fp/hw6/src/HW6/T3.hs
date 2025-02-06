module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid
  , startIllCell
  , simulate
  ) where

import Numeric.Natural
import System.Random (StdGen, mkStdGen, randomR)

import Data.Grid (Grid (..), exchangeCurrGrid, toBottomGrid, toLeftGrid, toRightGrid, toTopGrid)

import Control.Comonad (Comonad (..))

data Config = Config
  { probability      :: Double
  , incubationPeriod :: Natural
  , illnessDuration  :: Natural
  , immunityDuration :: Natural
  } deriving Show

data CellState
  = Healthy
  | Infected Natural
  | Ill Natural
  | Immune Natural
  deriving Show

data Cell = Cell
  { cellState :: CellState
  , cellRand  :: StdGen
  }

type Comonad19Grid = Grid Cell

simulateStartState :: Grid Int -> Comonad19Grid
simulateStartState g = exchangeCurrGrid (Cell (Infected 0) . mkStdGen $ extract g) $ fmap (Cell Healthy . mkStdGen) g

changeCell :: Config -> Cell -> Maybe Cell
changeCell (Config _ _ _ _) (Cell Healthy _) = Nothing
changeCell (Config _ inc _ _) (Cell (Infected i) r) = Just $ if (i < inc) then (Cell (Infected $ i + 1) r) else Cell (Ill 0) r
changeCell (Config _ _ ill _) (Cell (Ill i) r) = Just $ if (i < ill) then (Cell (Ill $ i + 1) r) else Cell (Immune 0) r
changeCell (Config _ _ _ imm) (Cell (Immune i) r) = Just $ if (i < imm) then (Cell (Immune $ i + 1) r) else Cell Healthy r

nextIllStage :: Double -> Cell -> [Bool] -> Cell
nextIllStage _ cell [] = cell
nextIllStage p cell (False:l) = nextIllStage p cell l
nextIllStage p cell (True:l) = let (prob, g1) = randomR (0 :: Double, 1) $ cellRand cell in if (p > prob) then (Cell (Infected 0) g1) else nextIllStage p (Cell Healthy g1) l

isCovidCarrier :: Cell -> Bool
isCovidCarrier (Cell (Infected _) _) = True
isCovidCarrier (Cell (Ill _) _)      = True
isCovidCarrier _                     = False

getNearCarrier :: Comonad19Grid -> [Bool]
getNearCarrier g = fmap (isCovidCarrier . extract) [toTopGrid g, toBottomGrid g, toLeftGrid g, toRightGrid g]

startIllCell :: Config -> Comonad19Grid -> Cell
startIllCell config@(Config p _ _ _) g = case changeCell config (extract g) of
  Just cell -> cell
  Nothing   -> nextIllStage p (extract g) (getNearCarrier g)

toNextStage :: Config -> Comonad19Grid -> Comonad19Grid
toNextStage c = extend (startIllCell c)

simulate :: Grid Int -> Config -> [Comonad19Grid]
simulate g c = iterate (toNextStage c) $ simulateStartState g
