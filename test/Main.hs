module Main where

import           Circuits.Network
import           Control.Lens

main :: IO ()
main = do
  let simple = empty "GND"
        & part "R1" .~ Just (Resistor 100 "N1" "GND")
        & part "C1" .~ Just (Capacitor 0.01 "N1" "N2")
        & part "Vs" .~ Just (CurrentSource 5 "GND" "N2")

  case compile simple of
    Left err -> putStrLn err
    Right c -> do
      let cs = take 100 $ iterate (step 0.01) c
      mapM_ print cs

