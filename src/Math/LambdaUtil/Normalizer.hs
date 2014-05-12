module Math.LambdaUtil.Normalizer
       (normalize) where

import Math.LambdaUtil.LambdaExpr

normalize :: Expr -> Expr
normalize (App a b) = case a' of
  Lambda name e -> normalize $ substitute e name b'
  _             -> App a' b'
  where
    a' = normalize a
    b' = normalize b
normalize (Lambda name e) = Lambda name (normalize e)
normalize (Var a)         = Var a

substitute :: Expr -- ^ Expression
           -> Name -- ^ Name of variable to substitute
           -> Expr -- ^ Substitution
           -> Expr
substitute (Var name) name' e
  | name == name' = e
  | otherwise     = Var name
substitute (Lambda name e) name' e'
  | name == name' = Lambda name e
  | otherwise     = Lambda name (substitute e name' e')
substitute (App a b) name e         = (substitute a name e) `App` (substitute b name e)
  
       
