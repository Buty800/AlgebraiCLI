module Main (main) where

import Test.HUnit
import qualified Data.Set as Set

import Classifier
import Parser
import Simplifier
import Solver
import Types

allTests :: Test
allTests = test [
    "Classifier" ~: testClassifier,
    "Parser" ~: testParse,
    "Simplifier" ~: testSimplifier,
    "Solver" ~: testSolver
  ]

testClassifier :: Test
testClassifier = test [
    vars (read "x+y+x_1y_2") ~?= Set.fromList [('x',-1),('y',-1),('y',2),('x',1)], 
    vars (read "(x^y+z)/2+z") ~?= Set.fromList [('x',-1),('y',-1),('z',-1)], 
    vars (read "(1+2^3)/(3-4)") ~?= Set.empty,
    isPolynomial (read "(x+1)(y^2-1)") ~?= True,
    --isPolynomial (read "x^x") ~?= False,
    isPolynomial (read "x^-1") ~?= False,
    isPolynomial (read "x^1.2") ~?= False
  ]

testParse :: Test 
testParse = test [
    read "1" ~?= Constant 1,
    read "2x_1+3y" ~?= Add (Mul (Constant 2) (Var ('x',1))) (Mul (Constant 3) (Var ('y',-1))) 
  ]

testSimplifier :: Test
testSimplifier = test [True ~?= True]

testSolver :: Test
testSolver = test [True ~?= True]

main :: IO ()
main = runTestTTAndExit allTests
