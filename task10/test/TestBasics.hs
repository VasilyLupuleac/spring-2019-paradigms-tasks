import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on infinite list" $
        head' [1..] @?= 1

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' works on infinite list" $
        take' 20 (tail' [1..]) @?= take' 20 [2..]

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' takes 4 elements from infinite list" $
        take' 4 [2..] @?= [2, 3, 4, 5]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' drops 2 elements from infinite list" $
        take' 20 (drop' 2 [3..]) @?= take' 20 [5..]

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]

    , testCase "filter' selects only numbers greater than 10 from all positive integers" $
        take' 20 (filter' (> 10) [1..]) @?= take' 20 [11..]

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works on a finite and an infinite list as expected" $
        take' 20 (concat' [1,2,3] [4..]) @?= take' 20 [1..]

    , testCase "concat' works on an infinite and a finite list as expected" $
        take' 20 (concat' [1..] [2,3]) @?= take' 20 [1..]

    , testCase "concat' works on two infinite lists as expected" $
        take' 20 (concat' [2..] [3..]) @?= take' 20 [2..]

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    ]