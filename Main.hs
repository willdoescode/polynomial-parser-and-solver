module Main where

import Text.Parsec
import Text.Parsec.String

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

data Signs
  = Plus
  | Minus
  deriving (Show, Eq)

data Tokens
  = Number Double
  | Operator Signs
  | Coefficient (Tokens, Tokens)
  | Exponent Char
  | Power (Tokens, Tokens)
  | Space
  deriving (Show, Eq)

power :: Parser Tokens
power = char '^' >> number

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

term :: Parser Tokens
term = do
  o <- opInFrontOfTerm
  num <- number <|> return (Number 1)
  var <- oneOf ['a' .. 'z']
  deg <- power <|> return (Number 1)
  return $
    Power
      ( Coefficient
          ( if o == Operator Minus
              then do
                let (Number n) = num
                Number $ n * (-1)
              else num,
            Exponent var
          ),
        deg
      )

whitespace :: Parser Tokens
whitespace = do
  many1 space
  return Space

terms :: Parser [Tokens]
terms =
  many1 $
    try whitespace
      <|> try term
      <|> try number
      <|> try op

parseTerms :: String -> [Tokens]
parseTerms input = case parse terms "" input of
  Left e -> error $ show e
  Right r -> filter (/= Space) r
