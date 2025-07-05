module Main (main) where

import Test.HUnit

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
testClassifier = test [True ~?= True]

testParse :: Test 
testParse = test [read "1" ~?= Constant 1]

testSimplifier :: Test
testSimplifier = test [True ~?= True]

testSolver :: Test
testSolver = test [True ~?= True]

main :: IO ()
main = runTestTTAndExit allTests
