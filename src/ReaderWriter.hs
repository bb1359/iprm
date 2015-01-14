{-|
Module      : Data types `a la carte - ReaderWriter
Description : Data types `a la carte IO operations implementation
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
module ReaderWriter
	(
	-- * Data Types
	Teletype(..),
	FileSystem(..),
	
	-- * Executions
	exec,
	execAlgebra,
	
	-- * Functions
	cat,
	wr,
	getChar,
	putChar,
	readFile,
	writeFile,
	getLine,
	putLine,
	dajChar,
	dobiChar
	)where
import Expr
import Term
import qualified Prelude (getLine,putChar,getChar,readFile,writeFile,putStrLn)
import Prelude hiding (getLine,putChar,getChar,readFile,writeFile,putStrLn)

-- SECTION 7

-- | Data Teletype
data Teletype a = GetChar (Char -> a) | PutChar Char a

-- | Data FileSystem
data FileSystem a = ReadFile FilePath (String -> a) | WriteFile FilePath String a

-- | Executions
exec :: Exec f => Term f a -> IO a
exec = foldTerm return execAlgebra

class Functor f => Exec f where
	-- | Execution Algebra, that executes the appropriate IO action
	execAlgebra :: f (IO a) -> IO a
	
-- | Exec for Teletype
instance Exec Teletype where
	execAlgebra (GetChar f) = Prelude.getChar >>= f
	execAlgebra (PutChar c io) = Prelude.putChar c >> io

-- | readFile executable	
cat::FilePath -> Term (Teletype :+: FileSystem) ()
cat fp = do
	contents <- readFile fp
	mapM putChar contents
	return()
	
-- | zapisiFile executable
-- | wr potDoDatotek+imeDatoteke VsebinaDatoteke
wr::FilePath -> String -> Term (Teletype :+: FileSystem) ()
wr wf str = do
	writeFile wf str
	
-- | Instance of functor for Teletype IO operations
instance Functor Teletype where
	fmap f (GetChar g) = GetChar (f . g)
	fmap f (PutChar c x) = PutChar c (f x)

-- | Instance of functor for FileSystem IO operations
instance Functor FileSystem where
	fmap f (ReadFile fp g) = ReadFile fp (f . g)
	fmap f (WriteFile fp g io) = WriteFile fp g (f io)

-- | getChar (read char from keyboard)	
getChar :: (Teletype :<: f) => Term f Char
getChar = inject2 (GetChar Pure)

-- | putChar (display char on screen)
putChar :: (Teletype :<: f) => Char -> Term f ()
putChar c = inject2 (PutChar c (Pure ()))

-- | readFile (read the content from the external file)
readFile :: (FileSystem :<: f) => FilePath -> Term f String
readFile fp = inject2 (ReadFile fp Pure)

-- | writeFile (write string to external file)
writeFile :: (FileSystem :<: f) => FilePath -> String -> Term f ()
writeFile fp str = inject2 (WriteFile fp str (Pure ()))

-- | Exec for FileSystem
instance Exec FileSystem where
	execAlgebra (ReadFile fp g) = Prelude.readFile fp >>= g
	execAlgebra (WriteFile fp str x) = Prelude.writeFile fp str >> x

-- | Exec for both Teletype and FileSystem
instance (Exec f, Exec g) => Exec (f :+: g) where
	execAlgebra (Inl x) = execAlgebra x
	execAlgebra (Inr y) = execAlgebra y
	
-- | getLine executable (wait and read user's input until enter is pressed)
getLine :: (Teletype :<: Teletype) => Term Teletype String
--getLine :: Term (Teletype) String
getLine = do
	c <- getChar
	if c == '\n'
		then return []
		else getLine >>= \cs -> return (c : cs)
	
-- | putLine executable	(display string to terminal)
putLine :: (Teletype :<: Teletype) => String -> Term Teletype ()
putLine c = do
	mapM putChar c
	return()

-- | putChar executable (display char to terminal)
dajChar :: (Teletype :<: Teletype) => Char -> Term Teletype ()
dajChar s = do
	putChar s
	return()
	
-- | getChar executable (wait and read user's input (one char only))
dobiChar :: (Teletype :<: Teletype) => Term Teletype Char
dobiChar = do
	getChar
	