module Main where

import Test.Hspec
import AlgebraischeDatentypen(adSpecifications);

allSpecifications :: [Spec]
allSpecifications =  adSpecifications

main::IO()
main = mapM_ hspec allSpecifications
