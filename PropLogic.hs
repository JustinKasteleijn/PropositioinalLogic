import Data.Maybe (fromMaybe)

data Formula 
  = Var Char 
  | Not Formula
  | Formula :∧:  Formula 
  | Formula :∨:  Formula
  | Formula :->:  Formula
  | Formula :<->: Formula  
 deriving (Show)

type Env = [(Char, Bool)] 
 
eval :: Formula -> Env -> Bool
eval (Var c) env      = fromMaybe False (lookup c env) 
eval (Not f) env      = not $ eval f env
eval (f :∧: f') env  = eval f env && eval f' env
eval (f :∨: f') env  = eval f env || eval f' env
eval (f :->: f') env  = not (eval f env) || eval f' env
eval (f :<->: f') env = ((not (eval f env)) && (not (eval f' env))) || ((eval f env) && (eval f' env))

tautology :: Formula -> Bool
tautology formula = all (\env -> eval formula env) permutations

permutations :: [Env] 
permutations = map (zip ['A'..'Z']) (replicate 24 [True, False])

main :: IO ()
main = do
  let formula1 = (Var 'A' :->: Var 'B') :∨: (Var 'B' :->: Var 'A')
  let formula2 = Var 'A' :∧: Not (Var 'A')
  
  print $ tautology formula1 
  print $ tautology formula2 
