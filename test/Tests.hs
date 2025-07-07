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
    --"Classifier" ~: testClassifier,
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
    read "2x_1+3y" ~?= Add [Mul [Constant 2, (Var ('x',1))], Mul [Constant 3, (Var ('y',-1))]],
    read "x_1+x/2" ~?= Add [Var ('x',1), Mul [Var ('x',-1), Inv (Constant 2)]], 
    read "y-x" ~?= Add [Var ('y',-1), Neg (Var ('x',-1))], 
    read "x_1-x/2" ~?= Add [Var ('x',1), Neg (Mul [Var ('x',-1), Inv (Constant 2)])],
    read "(x-1)(x-2)(x-3)" ~?= Mul [Add [Var ('x',-1), Neg (Constant 1)], Add [Var ('x',-1), Neg (Constant 2)], Add [Var ('x',-1), Neg (Constant 3)]],
    read "-2x^2-(1/2)x+1" ~?= Add [Mul[Constant (-2), Pow (Var ('x',-1)) (Constant 2)], Neg (Mul [Constant 1, Inv (Constant 2), Var ('x',-1)]), Constant 1], 
    read "-x_1+3x_2+2x_3" ~?= Add [Neg (Var ('x',1)), Mul [Constant 3, Var ('x',2)], Mul [Constant 2, Var ('x',3)]],
    read "x_12x_3" ~?= Mul [Var('x',12),Var('x',3)]
  ]

testSimplifier :: Test
testSimplifier = test [True ~?= True]

testSolver :: Test
testSolver = test [True ~?= True]

main :: IO ()
main = runTestTTAndExit allTests
