module DifferenceOfSquares exposing (..)


difference : Int -> Int
difference n =
    squareOfSum n - sumOfSquares n


squareOfSum : Int -> Int
squareOfSum n =
    List.sum (List.range 1 n) ^ 2


sumOfSquares : Int -> Int
sumOfSquares n =
    List.sum (List.map (\x -> x ^ 2) (List.range 1 n))
