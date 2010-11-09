module Main (main) where

import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Verbosity

import System.IO
import System.Process
import System.Directory

import Control.Monad

main :: IO ()
main = 
  do Just gcc <- programFindLocation gccProgram normal
     rawSystem gcc ["cbits/mkGmpDerivedConstants.c", "-m32", "-o", "dist/mkGmpDerivedConstants"]
     exists <- doesDirectoryExist "dist/include"
     unless exists $ createDirectory "dist/include/"
     header <- readProcess "dist/mkGmpDerivedConstants" [] ""
     writeFile "dist/include/GmpDerivedConstants.h" header
     defaultMain
