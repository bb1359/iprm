{-# LANGUAGE TypeOperators #-} 
-- Magicni ukaz da (:+:) deluje :) )
module Expr
    ( 
	(:+:)
	
    ) where

-- data Expr = Val Int | Add Expr Expr
data Expr f = In(f (Expr f))

data Val a = Val Int
type IntExpr = Expr Val

data Add e = Add e e
type AddExpr = Expr Add

data (f :+: g) e = Inl (f e) |  Inr (g e)

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

instance Functor Val where
	fmap f (Val x) = Val x
instance Functor Add where
	fmap f(Add e1 e2) = Add (f e1) (f e2)
	
instance (Functor f, Functor g) => Functor (f :+: g) where
	fmap f(Inl e1) = Inl (fmap f e1)
	fmap f(Inr e2) = Inr (fmap f e2)
	

foldExpr :: Functor f => (f a -> a) ->Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
	evalAlgebra::f Int -> Int
	
instance Eval Val where
	evalAlgebra (Val x) = x
	
instance Eval Add where
	evalAlgebra (Add x y) = x + y
	
instance (Eval f, Eval g) => Eval (f :+: g) where
	evalAlgebra(Inl x) = evalAlgebra x
	evalAlgebra(Inr y) = evalAlgebra y
	
eval:: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

-- Automating injections

val :: Int -> Expr Val
--val :: (Val :<: f) => Int Expr f
val x = In (Val x)

infixl 6 .+

(.+) :: Expr Add -> Expr Add -> Expr Add
--(.+) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x .+ y = In (Add x y)






