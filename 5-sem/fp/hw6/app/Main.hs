module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Grid (buildGridByFun, cutGrid)
import HW6.T3 (Cell (..), CellState (..), Comonad19Grid, Config (..), simulate)
import Numeric.Natural
import Options.Applicative
import Prettyprinter (Doc, annotate, line, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, putDoc)
import System.Console.ANSI (clearScreen)
import System.Random (initStdGen, randoms)

data Options = Options
  { optProbability :: Double
  , optIncub       :: Natural
  , optIll         :: Natural
  , optImmun       :: Natural
  , optGridSize    :: Integer
  , optIterations  :: Natural
  } deriving Show

optConfig :: Options -> Config
optConfig (Options p inc ill imm _ _) = (Config p inc ill imm)

optionsParser :: Parser Options
optionsParser = Options
  <$> option auto
    ( long "prob"
    <> help "Infection probability"
     <> metavar "DOUBLE" )
  <*> option auto
      ( long "incub"
     <> help "Incubation period duration (in days)"
     <> metavar "NATURAL" )
  <*> option auto
      ( long "ill"
     <> help "Illness duration (in days)"
     <> metavar "NATURAL" )
  <*> option auto
      ( long "immun"
     <> help "Immunity duration (in days)"
     <> metavar "NATURAL" )
  <*> option auto
      ( long "grid-size"
     <> help "Output grid size"
     <> metavar "INT" )
  <*> option auto
      ( long "iterations"
     <> help "Number of simulation iterations"
     <> metavar "NATURAL" )

optsParser :: ParserInfo Options
optsParser = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Simulate covid19 infection"
 <> header "example-program - a demonstration of optparse-applicative" )

prettyCell :: Cell -> Doc AnsiStyle
prettyCell (Cell Healthy _)      = pretty $ "_"
prettyCell (Cell (Infected _) _) = annotate (color Yellow) . pretty $ "i"
prettyCell (Cell (Ill _) _)      = annotate (color Red) . pretty $ "#"
prettyCell (Cell (Immune _) _)   = annotate (color Green) . pretty $ "@"

prettyGrid :: Integer -> Comonad19Grid -> Doc AnsiStyle
prettyGrid n = foldr1 (<>) . fmap (flip (<>) line . foldr1 (<>) . fmap prettyCell) . cutGrid (div n 2)

simulatePrint :: Integer -> [Comonad19Grid] -> IO ()
simulatePrint _ [] = return ()
simulatePrint s (cur : t) = do
    clearScreen
    liftIO $ putDoc $ prettyGrid s cur
    threadDelay 100000
    simulatePrint s t

code2Nums :: Int -> Int -> Int
code2Nums i j = let p = i * 3 + j * 2 - 4 in if p < 0 then i * i - p else p

main :: IO ()
main = do
    options <- execParser optsParser
    g <- initStdGen
    let r = randoms g
    let grid = buildGridByFun $ \i j -> r !! (code2Nums (fromInteger i) (fromInteger j))
    simulatePrint (optGridSize $ options) $ take (fromInteger . toInteger $ optIterations options) $ simulate grid $ optConfig options
