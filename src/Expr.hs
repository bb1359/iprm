{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Magicni ukaz da (:+:) in (:<:) deluje :) )
module Expr
    ( 
	(:+:),
	(:<:)
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

--val :: Int -> Expr Val
--val :: (Val :<: f) => Int Expr f
--val x = In (Val x)


-- section 4

infixl 6 .+
--infixl 6 .*

compose::[a->a]->a->a
compose fs v = foldl (flip (.)) id fs $ v

--(.+) :: Expr Add -> Expr Add -> Expr Add
--(.+) :: (Add :<: f) => Expr f -> Expr f -> Expr f
--x .+ y = In (Add x y)

--(.+)::(Add :<: f) => Expr f -> Expr f -> Expr f
--val::(Val :<: f) => Int -> Expr f

class (Functor sub, Functor sup) => sub :<: sup where
	inj::sub a -> sup a
	
instance Functor f => f :<: f where
	inj = id
instance (Functor f, Functor g) => f :<: (f :+: g) where
	inj = Inl
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
	inj = Inr . inj
	
inject::(g :<: f) => g (Expr f) -> Expr f
inject = In . inj
val::(Val :<: f) => Int -> Expr f
val x = inject(Val x)
(.+)::(Add :<: f) => Expr f -> Expr f -> Expr f
x .+ y = inject(Add x y)


-- section 5

data Mul x = Mul x x
instance Functor Mul where
	fmap f (Mul x y) = Mul (f x) (f y)
	
instance Eval Mul where
	evalAlgebra (Mul x y) = x * y

infixl 7 .*
(.*) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x .* y = inject (Mul x y)

class Render f where
	render :: Render g => f (Expr g) -> String
	
pretty :: Render f => Expr f -> String
pretty (In t) = render t

instance Render Val where
	render (Val i ) = show i
instance Render Add where
	render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
instance Render Mul where
	render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"
instance (Render f ,Render g) => Render (f :+: g) where
	render (Inl x ) = render x
	render (Inr y) = render y






