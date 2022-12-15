module StandardFormParser where

import General
import Text.Parsec
import Text.Parsec.String
import Tokens

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
