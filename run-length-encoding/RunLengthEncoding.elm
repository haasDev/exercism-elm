module RunLengthEncoding exposing (decode, encode, version)

import List exposing (drop, head, length)


version : Int
version =
    2


decode =
    always


encode : String -> String
encode sequence =
    -- Convert to List
    String.toList sequence
        -- group List items by consecutive letters
        |> groupSequence
        -- Print length of group followed by letter of group
        |> List.foldl


groupSequence : List Char -> List (List Char)
groupSequence chars =
    case head chars of
        Just c ->
            let
                seq =
                    takeWhile (\x -> x == c) chars

                rest =
                    drop (length seq) chars
            in
            [ seq ] ++ groupSequence rest

        Nothing ->
            []


takeWhile : (a -> Bool) -> List a -> List a
takeWhile fn items =
    case items of
        x :: xs ->
            if fn x then
                x :: takeWhile fn xs
            else
                []

        _ ->
            []
