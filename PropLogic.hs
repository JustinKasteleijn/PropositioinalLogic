data Formula 
  = Val Bool 
  | Not Formula
  | Formula :∧:  Formula 
  | Formula :∨:  Formula
  | Formula :->:  Formula
  | Formula :<->: Formula  
 deriving (Show)

eval :: Formula -> Bool
eval (Val b)        = b
eval (Not f)        = not $ eval f
eval (f :∧: f')     = eval f && eval f'
eval (f :∨: f')     = eval f || eval f'
eval (f :->: f')    = not (eval f) || eval f'
eval (f :<->: f')   = ((not (eval f)) && (not (eval f'))) || ((eval f) && (eval f'))

main :: IO ()
main = do
  let t = Val True
  let f = Val False

  print $ eval (t :∧: f)             -- False
  print $ eval (t :∨: f)             -- True
  print $ eval (Not f)               -- True
  print $ eval (t :->: f)            -- False
  print $ eval (f :->: t)            -- True
  print $ eval (t :<->: t)           -- True
  print $ eval (t :<->: f)           -- False
  print $ eval (Not (t :∨: f))       -- False
  print $ eval ((t :∧: f) :->: f)    -- True
  print $ eval ((t :∧: f) :<->: f)   -- True
