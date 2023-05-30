module Main where

import Cleff

import Hatch.Decode as Hatch
import Hatch.Encode as Hatch
import Hatch.Types

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  ccd <- TIO.readFile "examples/ccd-debit.ach"
  let ach = Hatch.encode ccd
  print ach
  print $ Hatch.decode ach
  
  
