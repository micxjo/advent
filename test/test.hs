{-# LANGUAGE OverloadedStrings #-}

import Advent.Day1
import Advent.Day2
import Advent.Day3
import Advent.Day4
import Advent.Day5

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests = testGroup "Advent of Code Tests" [day1, day2, day3, day4, day5]

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

day4 = testGroup "Day 4"
       [testCase "Part 1" $
        do hash "abcdef" 609043 @?= "000001DBBFA3A5C83A2D506429C7B00E"
           hash "pqrstuv" 1048970 @?= "000006136EF2FF3B291C85725F17325C"]

day5 = testGroup "Day 5"
       [testCase "Part 1" $
        do isNice "ugknbfddgicrmopn" @?= True
           isNice "aaa" @?= True
           isNice "lchzalrnumimnmhp" @?= False
           isNice "haegwjzuvuyypxyu" @?= False
           isNice "dvszwmarrgswjxmb" @?= False]
