{-# LANGUAGE GADTs #-}

-- import Control.Monad

-- Calculator language extended with an environment to hold defined variables

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  deriving (Show,Eq)

-- Value defintion for statically scoped eval

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

-- Enviornment for statically scoped eval

type Env = [(String,FBAEVal)]

-- divide operation for Int

divide :: Int -> Int -> Int
divide x y | x - y >= 0 = 1 + divide (x - y) y
           | otherwise = 0

-- eval operation for Int

evalInt :: FBAE -> Int
evalInt (Boolean b) = error "ERROR: Boolean Detected within Int operation!"
evalInt (And l r) = error "ERROR: Boolean Detected within Int operation!"
evalInt (Or l r) = error "ERROR: Boolean Detected within Int operation!"
evalInt (Leq l r) = error "ERROR: Boolean Detected within Int operation!"
evalInt (IsZero zero) = error "ERROR: Boolean Detected within Int operation!"
--evalInt

evalInt (Num n) =
  if n >= 0
    then n :: Int
    else error "ERROR: Only Natural Numbers are Allowed for Int operations"

evalInt (Plus l r) =
  let x = evalInt(l)
      y = evalInt(r)
      in x + y

evalInt (Minus l r) =
  let x = evalInt(l)
      y = evalInt(r)
      in if x >= y
        then x - y
        else error "ERROR: Resulting Difference must be Natural"

evalInt (Mult l r) =
  let x = evalInt(l)
      y = evalInt(r)
      in x * y

evalInt (Div l r) =
  let x = evalInt(l)
      y = evalInt(r)
      in if y == 0
        then error "ERROR: Cannot divide by '0'"
        else divide x y

-- eval operation for Bool

evalBool :: FBAE -> Bool
--evalBool (Num n) = evalBool ( And (Boolean True) (Num n) )
--  let x = And ( (Boolean True) (Num n) do {
--  error "ERROR: Int Detected!"
--  return Nothing
-- }
evalBool (Num n) = error "ERROR: Int Detected within Boolean Operation!"
evalBool (Plus l r) = error "ERROR: Int Detected within Boolean Operation!"
evalBool (Minus l r) = error "ERROR: Int Detected within Boolean Operation!"
evalBool (Mult l r) = error "ERROR: Int Detected within Boolean Operation!"
evalBool (Div l r) = error "ERROR: Int Detected within Boolean Operation!"

evalBool (Boolean b) = b
--  if b == True || b == False
--    then b
--    else error "ERROR: Only Boolean Values are Allowed within Boolean operations"

evalBool (And l r) =
  let x = evalBool(l)
      y = evalBool(r)
      in if x == True
        then y
        else False

evalBool (Or l r) =
  let x = evalBool(l)
      y = evalBool(r)
      in if x == True
        then x
        else y

evalBool (Leq l r) =
  let x = evalInt(l)
      y = evalInt(r)
      in if x <= y
        then True
        else False

evalBool (IsZero zero ) =
  if evalInt zero == 0
    then True
    else False

-- Statically scoped eval

evalM :: Env -> FBAE -> (Maybe FBAEVal)
evalM e (Num n) = Just (NumV (evalInt (Num n) ) )
evalM e (Plus l r) = Just (NumV (evalInt (Plus l r) ) )
evalM e (Minus l r) = Just (NumV (evalInt (Minus l r) ) )
evalM e (Mult l r) = Just (NumV (evalInt (Mult l r) ) )
evalM e (Div l r) = Just (NumV (evalInt (Div l r) ) )

-- evalM e (Bind i v b) = Just (ClosureV (Bind i v b) )
-- evalM e (Lambda i b) = Just (ClosureV (Lambda i b) )
-- evalM e (App f a) = Just (ClosureV (App f a) )
-- evalM e (Id i) = Just (ClosureV (Id i) )

evalM e (Boolean b) = Just (BooleanV (evalBool (Boolean b) ) )
evalM e (And l r) = Just (BooleanV (evalBool (And l r) ) )
evalM e (Or l r) = Just (BooleanV (evalBool (Or l r) ) )
evalM e (Leq l r) = Just (BooleanV (evalBool (Leq l r) ) )
evalM e (IsZero zero) = Just (BooleanV (evalBool (IsZero zero) ) )
evalM _ _ = Nothing

-- Type inference function

type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM _ _ = Nothing


-- Interpreter

interp :: FBAE -> (Maybe FBAEVal)
interp _ = Nothing

-- Factorial function for testing evalM and typeofM.  the type of test1 should
-- be TNum and the result of evaluating test1`should be (NumV 6).  Remember
-- that Just is used to return both the value and type.

test1 = (Bind "f" (Lambda "g" ((:->:) TNum TNum)
                    (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
                                         (Mult (Id "x")
                                               (App (Id "g")
                                                    (Minus (Id "x")
                                                           (Num 1)))))))
         (App (Fix (Id "f")) (Num 3)))
