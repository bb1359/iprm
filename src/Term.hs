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
	
foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure imp (Pure x) = pure x
foldTerm pure imp (Impure t) = imp (fmap (foldTerm pure imp) t)

inject2 :: (g :<: f) => g (Term f a) -> Term f a
inject2 = Impure . inj