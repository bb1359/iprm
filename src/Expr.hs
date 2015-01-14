{-|
Module      : Data types `a la carte - Expr
Description : Data types `a la carte expressions implementation
Copyright   : (c) Martin Frešer & Borja Bovcon, 2014
License     : unlicensed
Maintainer  : martin.freser@gmail.com, bb1359@student.uni-lj.si
Stability   : experimental


This is an implementation of expressions described in a scientific paper titled Data types `a la carte.

-}


{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DataKinds #-}

module Expr
    ( 
	-- * Data types
		Expr(..),
		Val(..),
		Add(..),
		Mul(..),
		Div(..),
		Mod(..),
		(:+:)(..),
		(:<:)(..),
	-- * Classes
		Render(..),
		Eval(..),
	-- * Basic operators
		val,
		(.+),
	-- * various functions
		eval,
		compose,
		pretty,
		foldExpr,
	-- * various examples
		addExample,
		rezSestevanje,
		rezMnozenje,
		rezDeljenje,
		rezModul,
		rezSestevanjeMnozenje
    ) where

infixr 7 :+:

data Expr f = In(f (Expr f))

data Val a = Val Int
type IntExpr = Expr Val

data Add e = Add e e
type AddExpr = Expr Add
-- | Coproduct of two signatures
data (f :+: g) e = Inl (f e) |  Inr (g e)

-- | Basic example, of how awful would it be, if we weren't come up with more clever solution
addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

--Those types are functors

-- | Instance of functor for value
instance Functor Val where
	fmap f (Val x) = Val x
	
-- | Instance of functor for addition
instance Functor Add where
	fmap f(Add e1 e2) = Add (f e1) (f e2)
	
-- | Instance of combined functors
instance (Functor f, Functor g) => Functor (f :+: g) where
	fmap f(Inl e1) = Inl (fmap f e1)
	fmap f(Inr e2) = Inr (fmap f e2)
	
-- | Folds the expression
foldExpr :: Functor f => (f a -> a) ->Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)
-- | Class for Evaluating Expr
class Functor f => Eval f where
	-- | Appropriate evaluating of given Integer
	evalAlgebra::f Int -> Int
	
-- | Evaluation of value
instance Eval Val where
	evalAlgebra (Val x) = x

-- | Evaluation of addition	
instance Eval Add where
	evalAlgebra (Add x y) = x + y
	
instance (Eval f, Eval g) => Eval (f :+: g) where
	evalAlgebra(Inl x) = evalAlgebra x
	evalAlgebra(Inr y) = evalAlgebra y
	
-- | Evaluation of expression
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
-- | With this type class, we won't have to write injections using Inl and Inr
class (Functor sub, Functor sup) => sub :<: sup where
	-- | injection
	inj::sub a -> sup a
--	prj::sub a -> Maybe (sub a)
	
--match::(g:<:f) => Expr f -> Maybe (g (Expr f))
--match(In t) = prj t
--distr::(Add :<: f,Mul :<: f) => Expr f -> Maybe (Expr f)
--distr t = do
--	Mul a b <- match t
--	Add c d <- match b
--	return(a .* c .+ a .* d)
-- | reflexivity	
instance Functor f => f :<: f where
	inj = id
-- | This explains, how to inject any value of type f a to a value of type (f :+: g) a, regardless of g.
instance (Functor f, Functor g) => f :<: (f :+: g) where
	inj = Inl
-- | we can inject f a into larger type (h :+: g) a by composing the first injection with an additional Inr.
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
	inj = Inr . inj

-- | inject function	
inject::(g :<: f) => g (Expr f) -> Expr f
inject = In . inj

-- | Set Expr from Int
val::(Val :<: f) => Int -> Expr f
val x = inject(Val x)

-- | Sum two Expr
(.+)::(Add :<: f) => Expr f -> Expr f -> Expr f
x .+ y = inject(Add x y)

--Section 5

-- | Instance for functor used for multiplication
data Mul x = Mul x x
instance Functor Mul where
	fmap f (Mul x y) = Mul (f x) (f y)
	
instance Eval Mul where
	evalAlgebra (Mul x y) = x * y

infixl 7 .*
(.*) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x .* y = inject (Mul x y)
--End: MUL

-- | Instance for functor used for division (int divison)
data Div x = Div x x
instance Functor Div where
	fmap f (Div x y) = Div (f x) (f y)
	
instance Eval Div where
	evalAlgebra (Div x y) = quot x y --quot je za celostevilsko deljenje

infixl 7 ./
(./) :: (Div :<: f) => Expr f -> Expr f -> Expr f
x ./ y = inject (Div x y)


-- | Instance for functor used for modulo
data Mod x = Mod x x

instance Functor Mod where
	fmap f (Mod x y) = Mod (f x) (f y)
	
instance Eval Mod where
	evalAlgebra (Mod x y) = mod x y
	
infixl 8 .%
(.%) :: (Mod :<: f) => Expr f -> Expr f -> Expr f
x .% y = inject (Mod x y)


-- | Rendering the output
class Render f where
	render :: Render g => f (Expr g) -> String
	
-- | Pretty the output
pretty :: Render f => Expr f -> String
pretty (In t) = render t

-- | Instance for rendering the value
instance Render Val where
	render (Val i ) = show i
	
-- | Instance for rendering the addition
instance Render Add where
	render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
	
-- | Instance for rendering the multiplication
instance Render Mul where
	render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"
	
-- | Instance for rendering the division
instance Render Div where
	render (Div x y) = "(" ++ pretty x ++ " / " ++ pretty y ++ ")"
	
-- | Instance for rendering the modulo
instance Render Mod where
	render (Mod x y) = "(" ++ pretty x ++ " % " ++ pretty y ++ ")"
	
-- | Instance for rendering
instance (Render f ,Render g) => Render (f :+: g) where
	render (Inl x ) = render x
	render (Inr y) = render y
--End: render & pretty

-- | Example of addition
rezSestevanje::Expr(Add :+: Val) = val 3 .+ val 1 .+ val 1

-- | Example of multiplication
rezMnozenje::Expr(Val :+: Mul) = val 3 .* val 2

-- | Example of division
rezDeljenje::Expr(Val :+: Div) = val 6 ./ val 3

-- | Example of modulo
rezModul::Expr(Val :+: Mod) = val 6 .% val 5


-- | Example of multiplication and addition
rezSestevanjeMnozenje::Expr(Val :+: Add :+: Mul) = (val 3) .* (val 2) .+ (val 4)