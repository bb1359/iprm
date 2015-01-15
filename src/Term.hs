{-|
Module      : Data types `a la carte - Term
Description : Data types `a la carte Term implementation
Copyright   : (c) Martin Frešer & Borja Bovcon, 2014
License     : unlicensed
Maintainer  : martin.freser@gmail.com, bb1359@student.uni-lj.si
Stability   : experimental


This is an implementation of two types of IO operations. Teletype operations - reading characters from keyboard and printing them on monitor and FileSystem operations - reading and writing from external file
-}

{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DataKinds #-}
module Term
	(
	Term(..),
	foldTerm,
	inject2
	)where
	
import Expr
-- | Monad, that we will be using
data Term f a =
	  Pure a
	| Impure (f (Term f a))
-- | Term f is a Functor
instance Functor f => Functor (Term f) where
	fmap f (Pure x) = Pure (f x)
	fmap f (Impure t) = Impure (fmap (fmap f) t)
-- | We declare that Term f is a Monad
instance Functor f => Monad (Term f) where
	return x 	= Pure x
	(Pure x) >>= f = f x
	(Impure t) >>= f = Impure (fmap (>>=f) t)
-- | fold to write functions over terms
foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure imp (Pure x) = pure x
foldTerm pure imp (Impure t) = imp (fmap (foldTerm pure imp) t)

-- | injection, so that we can cope with Terms
inject2 :: (g :<: f) => g (Term f a) -> Term f a
inject2 = Impure . inj