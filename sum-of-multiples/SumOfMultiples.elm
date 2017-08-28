module SumOfMultiples exposing (sumOfMultiples)


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples factors limit =
    let
        isFactor factors n =
            List.any (\x -> n % x == 0) factors
    in
    List.range 1 (limit - 1)
        |> List.filter (isFactor factors)
        |> List.sum
