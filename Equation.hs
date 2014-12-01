module Equation where


import Control.Applicative
import Data.Complex

data Equation = Equation [Formula] deriving (Show)

data Formula  = Sum [Formula] | Prod [Formula] | Uop UOp Formula |  Bop BOp Formula Formula | L (Complex Double) | Var Variable deriving (Eq, Show)

newtype Variable = V String deriving (Eq, Show)

newtype Subst = Subst [(Formula, Formula)] 

data BOp = Div | Exp | Log deriving (Eq, Show)

data UOp = Min deriving (Eq, Show)


test0 = Equation [L 3, Var (V "c"), Sum [L 5]]
test1 = subst (Subst [(var "c", Sum [var "d", L 1])]) test0
aterm = var "a"
bterm = var "b"
cterm = Sum [var "c", var "e"]
test2 = Prod [aterm, bterm, cterm]
term3 = distribute aterm cterm test2




var = Var . V


simplify :: Formula -> Formula
simplify (Sum  [x]) = x
simplify (Prod [x]) = x
simplify e          = e


distribute :: Formula -> Formula -> Formula -> Formula

distribute a c (Prod xs) = Prod $ go' xs
  where 
  	
  	go a (Sum xs) = Sum ((\x -> Prod [a, x]) <$> xs)
  	go a (Prod xs) = Prod (a:xs)
  	go a (Bop Div x1 x2) = Bop Div (Prod [a, x1]) x2
  	go a (Bop op x1 x2)  = error "undefined on distribute" 
  	go a (L x) = Prod [a, L x]
  	go a (Var x) = Prod [a, Var x]

  	go' (x:xs) | x == a = go' xs
  	go' (x:xs) | x == c = (go a c) : xs
  	go' (x:xs)  = x : go' xs
  	go' [] = []

distribute _ _ xs = error ("distribute is called on " ++ show xs) 



class Subable a where
	subst :: Subst -> a -> a
	subst (Subst su) e = foldr subst1 e su

	subst1 :: (Formula, Formula) -> a -> a



instance Subable Formula where
 	subst1 (x, e) y | x == y     = e 

 	subst1 su (Sum es) = Sum (subst1 su <$> es)
 	subst1 su (Prod es) = Prod (subst1 su <$> es)
 	subst1 su (Uop op e1) = Uop op (subst1 su e1) 
 	subst1 su (Bop op e1 e2) = Bop op (subst1 su e1) (subst1 su e2)
 	subst1 su (L x)       = L x
 	subst1 su (Var x)       = Var x

instance Subable Equation where
	subst1 su (Equation fs) = Equation (subst1 su <$> fs)


