{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DataKinds #-}
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

--Those types are functors
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

compose::[a->a]->a->a
compose fs v = foldl (flip (.)) id fs $ v

--(.+) :: Expr Add -> Expr Add -> Expr Add
--(.+) :: (Add :<: f) => Expr f -> Expr f -> Expr f
--x .+ y = In (Add x y)

--(.+)::(Add :<: f) => Expr f -> Expr f -> Expr f
--val::(Val :<: f) => Int -> Expr f

class (Functor sub, Functor sup) => sub :<: sup where
	inj::sub a -> sup a
--	prj::sub a -> Maybe (sub a)
	
--match::(g:<:f) => Expr f -> Maybe (g (Expr f))
--match(In t) = prj t
--distr::(Add :<: f,Mul :<: f) => Expr f -> Maybe (Expr f)
--distr t = do
--	Mul a b <- match t
--	Add c d <- match b
--	return(a .* c .+ a .* d)
	
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

--Section 5

--Begin: MUL
data Mul x = Mul x x
instance Functor Mul where
	fmap f (Mul x y) = Mul (f x) (f y)
	
instance Eval Mul where
	evalAlgebra (Mul x y) = x * y

infixl 7 .*
(.*) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x .* y = inject (Mul x y)
--End: MUL

--Begin: DIV
data Div x = Div x x

instance Functor Div where
	fmap f (Div x y) = Div (f x) (f y)
	
instance Eval Div where
	evalAlgebra (Div x y) = quot x y --quot je za celostevilsko deljenje

infixl 7 ./
(./) :: (Div :<: f) => Expr f -> Expr f -> Expr f
x ./ y = inject (Div x y)
--End: DIV

--Begin: MOD
data Mod x = Mod x x

instance Functor Mod where
	fmap f (Mod x y) = Mod (f x) (f y)
	
instance Eval Mod where
	evalAlgebra (Mod x y) = mod x y
	
infixl 8 .%
(.%) :: (Mod :<: f) => Expr f -> Expr f -> Expr f
x .% y = inject (Mod x y)
--End: MOD


--Begin: render & pretty
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
instance Render Div where
	render (Div x y) = "(" ++ pretty x ++ " / " ++ pretty y ++ ")"
instance Render Mod where
	render (Mod x y) = "(" ++ pretty x ++ " % " ++ pretty y ++ ")"
instance (Render f ,Render g) => Render (f :+: g) where
	render (Inl x ) = render x
	render (Inr y) = render y
--End: render & pretty

--Primer samo sestevanja:
rezSestevanje::Expr(Add :+: Val) = val 3 .+ val 1 .+ val 1

--Primer samo mnozenja
rezMnozenje::Expr(Val :+: Mul) = val 3 .* val 2

--Primer samo deljenja
rezDeljenje::Expr(Val :+: Div) = val 6 ./ val 3

--Primer samo modula
rezModul::Expr(Val :+: Mod) = val 6 .% val 5

--Primer sestevanja in mnozenja
--rezSestevanjeMnozenje::Expr(Val :+: Add :+: Mul) = (val 3) .* (val 2) .+ (val 4)


--section 7

data Term f a =
	  Pure a
	| Impure (f (Term f a))
	
instance Functor f => Functor (Term f) where
	fmap f (Pure x) = Pure (f x)
	fmap f (Impure t) = Impure (fmap (fmap f) t)

instance Functor f => Monad (Term f) where
	return x 	= Pure x
	(Pure x) >>= f = f x
	(Impure t) >>= f = Impure (fmap (>>=f) t)
	

data Incr t = Incr Int t
data Recall t = Recall (Int -> t)

instance Functor Incr where
	fmap f (Incr int t) = Incr int (f t)
--instance Functor Recall where
--	fmap f (Recall (int -> t)) = Recall (f int -> fmap f t)

inject2 :: (g :<: f) => g (Term f a) -> Term f a
inject2 = Impure . inj

incr :: (Incr :<: f) => Int -> Term f ()
incr i = inject2 (Incr i (Pure ()))

recall :: (Recall :<: f) => Term f Int
recall = inject2 (Recall Pure)

--tick :: Term (Recall :+: Incr) Int
--tick = do
--	y <- recall
--	incr 1
--	return y

foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure imp (Pure x) = pure x
foldTerm pure imp (Impure t) = imp (fmap (foldTerm pure imp) t)

newtype Mem = Mem Int

class Functor f => Run f where
	runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))
	
instance Run Incr where
	runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))
--instance Run Recall where
--	runAlgebra (Recall r) (Mem i) = r i (Mem i)
instance (Run f, Run g) => Run (f :+: g) where
	runAlgebra (Inl r) = runAlgebra r
	runAlgebra (Inr r) = runAlgebra r
	
run :: Run f => Term f a -> Mem -> (a, Mem)
run = foldTerm (,) runAlgebra
