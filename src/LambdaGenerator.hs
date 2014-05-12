module Main where

import Control.Monad.Random

import Math.LambdaUtil.Generator

main = (putStrLn . show) =<< evalRandIO (generateExpr defaultParams)
