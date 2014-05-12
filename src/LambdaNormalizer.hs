module Main where

import Math.LambdaUtil.Parser
import Math.LambdaUtil.Normalizer

main = do
  s <- parseExpr `fmap` getContents
  putStrLn $ show $ normalize s
