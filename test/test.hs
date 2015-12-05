{-# LANGUAGE OverloadedStrings #-}

import Advent.Day1
import Advent.Day2
import Advent.Day3

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "Advent of Code Tests" [day1, day2, day3]

day1 = testGroup "Day 1"
       [testCase "Part 1" $
        do santaFloor "(())" @?= 0
           santaFloor "()()" @?= 0
           santaFloor "(((" @?= 3
           santaFloor "(()(()(" @?= 3
           santaFloor "))(((((" @?= 3
           santaFloor "())" @?= (-1)
           santaFloor "))(" @?= (-1)
           santaFloor ")))" @?= (-3)
           santaFloor ")())())" @?= (-3)
       ,testCase "Part 2" $
       do firstBasement ")" @?= Just 1
          firstBasement "()())" @?= Just 5]

day2 = testGroup "Day 2"
       [testCase "Part 1" $
        do wrappingPaper 2 3 4 @?= 58
           wrappingPaper 1 1 10 @?= 43
       ,testCase "Part 2" $
       do ribbon 2 3 4 @?= 34
          ribbon 1 1 10 @?= 14]

day3 = testGroup "Day 3"
       [testCase "Part 1" $
        do countDeliveries ">" @?= 2
           countDeliveries "^>v<" @?= 4
           countDeliveries "^v^v^v^v^v" @?= 2
       ,testCase "Part 2" $
       do countRoboDeliveries "^>" @?= 3
          countRoboDeliveries "^>b<" @?= 3
          countRoboDeliveries "^v^v^v^v^v" @?= 11]
