module General where

import Text.Parsec
import Text.Parsec.String
import Tokens

data SolutionType
  = NoRealSolutions
  | OneSolution
  | TwoSolutions
  deriving (Eq)

instance Show SolutionType where
  show NoRealSolutions = "no real solutions"
  show OneSolution = "one solution"
  show TwoSolutions = "two solutions"

typeOfSolutions :: Double -> SolutionType
typeOfSolutions disc
  | disc < 0 = NoRealSolutions
  | disc == 0 = OneSolution
  | disc > 0 = TwoSolutions
  | otherwise = NoRealSolutions

extractNumAndPow :: Tokens -> (Double, Double)
extractNumAndPow
  ( Power
      ( Coefficient
          ( Number a,
            _
            ),
        Number b
        )
    ) = (a, b)
extractNumAndPow _ = error "No equation provided"

whitespace :: Parser Tokens
whitespace = do
  many1 space
  return Space

op :: Parser Tokens
op = do
  sign <- oneOf "+-"
  return $
    Operator
      ( if sign == '-'
          then Minus
          else Plus
      )

opInFrontOfTerm :: Parser Tokens
opInFrontOfTerm = do
  o <- try op <|> return (Operator Plus)
  _ <- try whitespace <|> return Space
  return o

number :: Parser Tokens
number = do
  o <- opInFrontOfTerm
  n <- many1 digit
  if o == Operator Minus
    then return $ Number (read n * (-1))
    else return $ Number (read n)

power :: Parser Tokens
power = char '^' >> number
