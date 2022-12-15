module Main where

import General
import StandardFormParser
import Text.Parsec
import Text.Parsec.String
import Tokens

main :: IO ()
main = do
  if typeOfSol == NoRealSolutions
    then putStrLn outText
    else putStrLn $ outText ++ " which are: " ++ show r1 ++ " and " ++ show r2

  print t
  print (r1, r2, discrim)
  where
    equation = "x^2 - 8x + 12"
    t = parseTerms equation
    (r1, r2, discrim) = solve t
    typeOfSol = typeOfSolutions discrim
    outText = "The equation " ++ equation ++ " has " ++ show typeOfSol

solve ::
  [Tokens] ->
  ( Double, -- Root 1
    Double, -- Root 2
    Double -- Discriminant
  )
solve
  [a', b', Number c] =
    ( (- b + sqrt discrim) / bt,
      (- b - sqrt discrim) / bt,
      discrim
    )
    where
      (a, _) = extractNumAndPow a'
      (b, _) = extractNumAndPow b'
      discrim = b ^ 2 - 4 * a * c
      bt = 2 * a
solve _ = error "No equation provided"
