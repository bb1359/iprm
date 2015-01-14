{-# LANGUAGE TypeOperators #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DataKinds #-}
module ReaderWriter
	(
	)where
import Expr
import Term
import qualified Prelude (getLine,putChar,getChar,readFile,writeFile,putStrLn)
import Prelude hiding (getLine,putChar,getChar,readFile,writeFile,putStrLn)

-- SECTION 7

-- teletype + filesystem
data Teletype a = GetChar (Char -> a) | PutChar Char a

data FileSystem a = ReadFile FilePath (String -> a) | WriteFile FilePath String a

exec :: Exec f => Term f a -> IO a
exec = foldTerm return execAlgebra

class Functor f => Exec f where
	execAlgebra :: f (IO a) -> IO a
	
instance Exec Teletype where
	execAlgebra (GetChar f) = Prelude.getChar >>= f
	execAlgebra (PutChar c io) = Prelude.putChar c >> io

-- readFile executable	
cat::FilePath -> Term (Teletype :+: FileSystem) ()
cat fp = do
	contents <- readFile fp
	mapM putChar contents
	return()
	
-- zapisiFile executable
-- wr potDoDatotek+imeDatoteke VsebinaDatoteke
wr::FilePath -> String -> Term (Teletype :+: FileSystem) ()
wr wf str = do
	writeFile wf str
	
	
instance Functor Teletype where
	fmap f (GetChar g) = GetChar (f . g)
	fmap f (PutChar c x) = PutChar c (f x)
	
instance Functor FileSystem where
	fmap f (ReadFile fp g) = ReadFile fp (f . g)
	fmap f (WriteFile fp g io) = WriteFile fp g (f io)
	
getChar :: (Teletype :<: f) => Term f Char
getChar = inject2 (GetChar Pure)

putChar :: (Teletype :<: f) => Char -> Term f ()
putChar c = inject2 (PutChar c (Pure ()))


readFile :: (FileSystem :<: f) => FilePath -> Term f String
readFile fp = inject2 (ReadFile fp Pure)

writeFile :: (FileSystem :<: f) => FilePath -> String -> Term f ()
writeFile fp str = inject2 (WriteFile fp str (Pure ()))

instance Exec FileSystem where
	execAlgebra (ReadFile fp g) = Prelude.readFile fp >>= g
	execAlgebra (WriteFile fp str x) = Prelude.writeFile fp str >> x
	
instance (Exec f, Exec g) => Exec (f :+: g) where
	execAlgebra (Inl x) = execAlgebra x
	execAlgebra (Inr y) = execAlgebra y
	
-- getLine executable
getLine :: (Teletype :<: Teletype) => Term Teletype String
--getLine :: Term (Teletype) String
getLine = do
	c <- getChar
	if c == '\n'
		then return []
		else getLine >>= \cs -> return (c : cs)
	
-- putLine executable	
putLine :: (Teletype :<: Teletype) => String -> Term Teletype ()
putLine c = do
	mapM putChar c
	return()

-- putChar executable
dajChar :: (Teletype :<: Teletype) => Char -> Term Teletype ()
dajChar s = do
	putChar s
	return()
	
-- getChar executable
dobiChar :: (Teletype :<: Teletype) => Term Teletype Char
dobiChar = do
	getChar

		
--putLine :: String -> Term (Teletype) ()
--putLine s = do
--	mapM putChar s
--	return()
	