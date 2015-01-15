{-|
Module      : Data types `a la carte - Main
Description : Data types `a la carte implementation
Copyright   : (c) Martin Frešer & Borja Bovcon, 2014
License     : unlicensed
Maintainer  : martin.freser@gmail.com, bb1359@student.uni-lj.si
Stability   : experimental


This is an implementation of a scientific paper titled Data types `a la carte.
It includes a simple calculator with operations: addition, subtraction, multiplication, division and memory
It also includes two types of IO operations. Teletype operations - reading characters from keyboard and printing them on monitor and FileSystem operations - reading and writing from external file
-}

module Main
	(
	)where
import Calculator
import ReaderWriter
import Expr
import Term

import Prelude hiding (getLine,putChar,getChar,readFile,writeFile,putStrLn)