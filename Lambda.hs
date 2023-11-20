module Lambda where

import Expr
import Data.List

-- TODO 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars (Variable x) = [x]
free_vars (Function x e) = filter (/= x) $ free_vars e
free_vars (Application e1 e2) = removeDuplicates $ free_vars e1 ++ free_vars e2
  where removeDuplicates [] = []
        removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- TODO 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce (Variable e1) x e2 = if e1 == x then e2 else Variable e1
reduce (Application e11 e12) x e2 = Application (reduce e11 x e2) (reduce e12 x e2)
reduce (Function y body) x e2
  | y == x = Function y body
  | y `elem` free_vars e2 = Function newVarName (reduce (substituteVar y newVarName body) x e2)
  | otherwise = Function y (reduce body x e2)
  where newVarName = new_var_name "x" (Application (Function y body) e2)

substituteVar :: String -> String -> Expr -> Expr
substituteVar old new (Variable x) = if x == old then Variable new else Variable x
substituteVar old new (Function y body) = if y == old then Function y body else Function y (substituteVar old new body)
substituteVar old new (Application e1 e2) = Application (substituteVar old new e1) (substituteVar old new e2)

new_var_name :: String -> Expr -> String
new_var_name y e = head $ filter (\x -> notElem x (free_vars e ++ [y])) vars
  where vars = map (\n -> y ++ show n) [1..]
  
-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Variable x) = (Variable x)
stepN (Function x e) = (Function x (stepN e))
stepN (Application (Function x e1) e2) = reduce e1 x e2
stepN (Application e1 e2) = if (hasRedex e1) then Application (stepN e1) e2 else Application e1 (stepN e2)


hasRedex :: Expr -> Bool
hasRedex (Variable _) = False
hasRedex (Function _ body) = hasRedex body
hasRedex (Application (Function _ _) _) = True
hasRedex (Application e1 e2) = hasRedex e1 || hasRedex e2

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN e = if (hasRedex e) then reduceN (stepN e) else e

reduceAllN :: Expr -> [Expr]
reduceAllN e = if (hasRedex e) then e : reduceAllN (stepN e) else [e]

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Variable x) = (Variable x)
stepA (Function x e) = Function x (stepA e)
stepA (Application (Function x e1) e2) = if (hasRedex e2) then (Application (Function x e1) (stepA e2)) else (reduce e1 x e2)
stepA (Application e1 e2) = if (hasRedex e1) then (Application (stepA e1) e2) else (Application e1 (stepA e2))

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA e = if (hasRedex e) then reduceA (stepA e) else e

reduceAllA :: Expr -> [Expr]
reduceAllA e = if (hasRedex e) then e : reduceAllA (stepA e) else [e]

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros context (Variable x) = Variable x
evalMacros context (Function x e) = Function x (evalMacros context e)
evalMacros context (Application e1 e2) = Application (evalMacros context e1) (evalMacros context e2)
evalMacros context (Macro m) =
  case lookup m context of
    Just e -> evalMacros context e
    Nothing -> Macro m

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strat list = evalHelper strat list []
  where
    evalHelper strat [] context = []
    evalHelper strat (x : xs) context = case x of
      Evaluate expr -> (strat (evalMacros context expr)) : (evalHelper strat xs context) 
      Assign name expr -> evalHelper strat xs (updateContext name expr context)
        where 
          updateContext name expr [] = [(name, expr)]
          updateContext name expr ((n,e) : cx)
            | name == n = (n, expr) : cx
            | otherwise = (n, e) : (updateContext name expr cx)
