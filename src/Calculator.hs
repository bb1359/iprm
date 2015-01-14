{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DataKinds #-}
module Calculator 
	(
	
	)where
import Term
import Expr
	

data Incr t = Incr Int t
data Recall t = Recall (Int -> t)

instance Functor Incr where
	fmap f (Incr int t) = Incr int (f t)

instance Functor Recall where
	fmap f (Recall rc) = Recall (f . rc)

incr :: (Incr :<: f) => Int -> Term f ()
incr i = inject2 (Incr i (Pure ()))

recall :: (Recall :<: f) => Term f Int
recall = inject2 (Recall Pure)

tick :: Term (Recall :+: Incr) Int
tick = do
	y <- recall
	incr 1
	return y



newtype Mem = Mem Int

class Functor f => Run f where
	runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))
	
instance Run Incr where
	runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))

instance Run Recall where
	runAlgebra (Recall r) (Mem i) = r i (Mem i)
	
instance (Run f, Run g) => Run (f :+: g) where
	runAlgebra (Inl r) = runAlgebra r
	runAlgebra (Inr r) = runAlgebra r
	
instance Show Mem where
	show (Mem m) = "Current memory: " ++ show m
	
run :: Run f => Term f a -> Mem -> (a, Mem)
run = foldTerm (,) runAlgebra
