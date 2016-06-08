module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st s i = rs
  where
    rs :: State
    rs x
      | x==s = i
      | otherwise = st x   

empty :: State
empty = const 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var s) = st s
evalE _ (Val i) = i 
evalE st (Op a op b) = case op of
  Plus -> x + y
  Minus -> x - y
  Times -> x * y
  Divide -> div x y
  Gt -> if x > y then 1 else 0
  Ge -> if x >= y then 1 else 0
  Lt -> if x < y then 1 else 0
  Le -> if x <= y then 1 else 0
  Eql -> if x ==y then 1 else 0
  where
    x = evalE st a
    y = evalE st b
-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e) =  DAssign s e
desugar (Incr s) = DAssign s (Op (Var s) Plus (Val 1))
desugar (If e a b) = DIf e (desugar a) (desugar b)
desugar (While e a) = DWhile e (desugar a)
desugar (For a e b c) = DSequence (desugar a) (DWhile e (DSequence (desugar c) (desugar b)))
desugar (Sequence a b) = DSequence (desugar a) (desugar b)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s e) = extend st s (evalE st e) 
evalSimple st (DIf e a b) = if evalE st e /= 0 then evalSimple st a else evalSimple st b
evalSimple st (DWhile e a) = if evalE st e /= 0 then evalSimple newS (DWhile e a) else st
  where newS = evalSimple st a
evalSimple st (DSequence a b) = stB
  where stA = evalSimple st a
        stB = evalSimple stA b
evalSimple st DSkip = st
run :: State -> Statement -> State
run  s sm = evalSimple s $ desugar sm

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]