{-|
Module      : Data types `a la carte - Calculator
Description : Data types `a la carte's calculator implementation
Copyright   : (c) Martin Frešer & Borja Bovcon, 2014
License     : unlicensed
Maintainer  : martin.freser@gmail.com, bb1359@student.uni-lj.si
Stability   : experimental

This is an implementation of a simple calculator with operations: addition, subtraction, multiplication, division and memory.
-}

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

-- | Instance for functor used for incrementation of the specified number
instance Functor Incr where
	fmap f (Incr int t) = Incr int (f t)

-- | Instance for functor used for restoring the current number in memory
instance Functor Recall where
	fmap f (Recall rc) = Recall (f . rc)

-- | Incrementing the number
incr :: (Incr :<: f) => Int -> Term f ()
incr i = inject2 (Incr i (Pure ()))

-- | Restoring (Recalling) the number stored in memory
recall :: (Recall :<: f) => Term f Int
recall = inject2 (Recall Pure)

-- | Tick. Reads the number from memory (with recall) and increments it (with incr).
tick :: Term (Recall :+: Incr) Int
tick = do
	y <- recall
	incr 1
	return y


-- | Used for storing the number in memory.
newtype Mem = Mem Int

class Functor f => Run f where
	runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))

-- | Instance Run for Incrementation.	
instance Run Incr where
	runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))

-- | Instance Run for Recall.
instance Run Recall where
	runAlgebra (Recall r) (Mem i) = r i (Mem i)
	
-- | Instance Run for Incrementation and Recall.
instance (Run f, Run g) => Run (f :+: g) where
	runAlgebra (Inl r) = runAlgebra r
	runAlgebra (Inr r) = runAlgebra r

-- | Instance show. It shows the current number stored in the calculator's memory.	
instance Show Mem where
	show (Mem m) = "Current memory: " ++ show m
	

-- | Run. This makes our calculator executable for incr and recall.	
run :: Run f => Term f a -> Mem -> (a, Mem)
run = foldTerm (,) runAlgebra
