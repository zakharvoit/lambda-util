module Math.LambdaUtil.LambdaExpr
       (Name,
        Expr(..)) where

type Name = String

data Expr = Var Name
          | Lambda Name Expr
          | App Expr Expr

instance Show Expr where
  show (Var name)      = name
  show (Lambda name e) = "\\" ++ name ++ " . " ++ (show e)
  show (App a b) = (showA a) ++ " " ++ (showB b)
                   where
                     showA a@(Lambda _ _) = "(" ++ show a ++ ")"
                     showA a              = show a
                     showB b@(Lambda _ _) = "(" ++ show b ++ ")"
                     showB b@(App _ _)    = "(" ++ show b ++ ")"
                     showB b              = show b
                     
