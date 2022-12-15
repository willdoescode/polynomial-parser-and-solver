module Tokens where

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
