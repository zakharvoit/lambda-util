module Math.LambdaUtil.Generator
       (generateExpr
       , defaultParams) where

import Control.Monad.Random
import Control.Applicative
import Data.Functor

import Math.LambdaUtil.LambdaExpr

data GenParams = GenParams { depth :: Int   -- ^ Max depth of expression
                           , vars :: Int -- ^ Max number of variables in expression
                           }

defaultParams :: GenParams
defaultParams = GenParams 3 3

generateExpr :: RandomGen g =>
                GenParams
                -> Rand g Expr
generateExpr (GenParams d v) = generate [] d v

generate :: RandomGen g =>
            [Name]          -- ^ Defined variables
            -> Int          -- ^ Rest depth
            -> Int          -- ^ Rest vars
            -> Rand g Expr  -- ^ Random lambda expression
generate names depth vars
  | (length names) == 0 = do
    f <- randomChoice [generateApp, generateLambda]
    f names depth vars
  | depth <= 0 = generateVar names
  | vars  == 0 = generateApp names depth vars
  | otherwise = do
    f <- randomChoice [\n _ _ -> generateVar n, generateApp, generateLambda]
    f names depth vars

generateVar :: RandomGen g =>
               [Name]
               -> Rand g Expr
generateVar names = Var <$> (randomChoice names)

generateApp :: RandomGen g =>
               [Name]
               -> Int
               -> Int
               -> Rand g Expr
generateApp n d v = (App <$> (generate n (d - 1) v)) <*> (generate n (d - 1) v)

generateLambda :: RandomGen g =>
                  [Name]
                  -> Int
                  -> Int
                  -> Rand g Expr
generateLambda n d v = Lambda newVar <$> generate (newVar:n) (d - 1) (v - 1)
  where
    newVar = notIn n

randomChoice :: RandomGen g =>
                [a]
                -> Rand g a
randomChoice [] = error "List is empty"
randomChoice a = (a !!) <$> getRandomR (0, (length a) - 1)

notIn :: [Name] -> Name
notIn names = head $ filter (`notElem` names) vars'
  where
    vars = ["x", "y", "z", "a", "b", "c"]
    vars' = vars ++ (iterate (++"'") (head vars))
