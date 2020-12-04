-- file Name: project4.hs
-- file Author: Marco Borth, 2894114
-- description: project4 file containing Fix and Type Checking
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

elabFBAEC :: FBAE -> FBAE
--elabFBAEC (Lambda f t a) = Lambda f (typeofM "type" f) (elabFBAEC a)
elabFBAEC (App f a) = App (elabFBAEC f) (elabFBAEC a)
--elabFBAEC (Bind i v b) = App (Lambda i (typeofM "type" v) (elabFBAEC v)) (elabFBAEC b)
elabFBAEC (Id i) = Id i

subst::String -> FBAE -> FBAE -> FBAE
subst i v (Num n) = (Num n)
subst i v (Plus l r) = Plus (subst i v l) (subst i v r) --Num (evalAE (Plus l r)) -- Plus (subst x v l) (subst x v r)
subst i v (Minus l r) = Minus (subst i v l) (subst i v r) --Num(evalAE (Minus l r))
subst i v (Mult l r) = Mult (subst i v l) (subst i v r)
subst i v (Div l r) = Div (subst i v l) (subst i v r)
subst i v (Bind i' v' b') = --Bind a (subst x v b) (subst x v c)
  if i == i'
    then Bind i' (subst i v v') b'
    else Bind i' (subst i v v') (subst i v b')

-- eval operation for Int

evalInt :: FBAE -> Int
evalInt (Boolean b) = error "ERROR: Boolean Detected within Int operation!"
evalInt (And l r) = error "ERROR: Boolean Detected within Int operation!"
evalInt (Or l r) = error "ERROR: Boolean Detected within Int operation!"
evalInt (Leq l r) = error "ERROR: Boolean Detected within Int operation!"
evalInt (IsZero zero) = error "ERROR: Boolean Detected within Int operation!"

evalInt (Num n) =
  if n >= 0
    then n :: Int
    else error "ERROR: Only Natural Numbers are Allowed"

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

-- eval operation for Boolean

evalBool :: FBAE -> Bool
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
evalM envi (Num n) =
  if n >= 0
    then Just (NumV n )
    else error "ERROR: Only Natural Numbers are Allowed for Int operations"

evalM envi (Plus l r) =
  let x = evalInt l
      y = evalInt r
      in Just (NumV (x + y))


evalM envi (Minus l r) =
  let x = evalInt l
      y = evalInt r
      in if x >= y
        then Just (NumV (x - y))
        else error "ERROR: Resulting Difference must be Natural"

evalM envi (Mult l r) =
  let x = evalInt l
      y = evalInt r
      in Just (NumV (x * y))

evalM envi (Div l r) =
  let x = evalInt l
      y = evalInt r
      in if y == 0
        then error "ERROR: Cannot divide by '0'"
        else Just (NumV (divide x y))

evalM envi (Bind i v b) = Just (ClosureV "Bind" (Bind i v b) envi)
evalM envi (Lambda i t b) = Just (ClosureV "Lambda" (Lambda i t b) envi)
evalM envi (App f a) =  Just (ClosureV "App" (App f a) envi)  -- evalM envi (elabFBAEC x)
evalM envi (Id i) = Just (ClosureV "Id" (Id i) envi)

evalM envi (Boolean b) = Just (BooleanV (evalBool (Boolean b) ) )
evalM envi (And l r) = Just (BooleanV (evalBool (And l r) ) )
evalM envi (Or l r) = Just (BooleanV (evalBool (Or l r) ) )
evalM envi (Leq l r) = Just (BooleanV (evalBool (Leq l r) ) )
evalM envi (IsZero zero) = Just (BooleanV (evalBool (IsZero zero) ) )

evalM envi (If c t e ) =
  if evalM envi c == Just (BooleanV True)
    then evalM envi t
    else evalM envi e

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
