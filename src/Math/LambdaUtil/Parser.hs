module Math.LambdaUtil.Parser
       (parseExpr) where

import Data.Functor

import Text.ParserCombinators.Parsec

import Math.LambdaUtil.LambdaExpr

parseExpr :: String -> Expr
parseExpr s = case res of
  Left e     -> error (show e)
  Right res' -> res'
  where
    res = parse parseLambdaExpr "" s

parseAllExpr = do
  p <- parseLambdaExpr
  eof
  return p
  
parseLambdaExpr = do
  res <- parseLambda <|> parseMultiApp
  spaces
  return res

parens = try . parens'
parens' = between ((char '(')) ((char ')'))

parseLambda = try parseLambda'
parseLambda' = do
  spaces
  (char '\\') <|> (char 'λ')
  spaces
  (Var name) <- parseVar
  spaces
  (char '.') <|> (char '-' >> char '>')
  spaces
  e <- parseLambdaExpr
  return $ Lambda name e

parseMultiApp = try parseMultiApp'
parseMultiApp' = do
  a <- parseAtom
  s <- many parseAtom
  return $ foldl App a s

parseAtom = try $ spaces >> (parseVar <|> (parens parseLambdaExpr))

parseVar = try $ spaces >> (Var <$> (many1 letter))
