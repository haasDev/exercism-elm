module RunLengthEncoding exposing (decode, encode, version)

import List exposing (drop, head, length)
import Regex exposing (regex)


version : Int
version =
    2


createDecoding list =
    case list of
        x :: y :: rest ->
            case String.toInt x of
                Ok num ->
                    String.repeat num y ++ createDecoding rest

                Err _ ->
                    x ++ createDecoding (y :: rest)

        x :: rest ->
            x ++ createDecoding rest

        [] ->
            ""


decode string =
    string
        |> Regex.split Regex.All (regex "(\\d+)(\\D)")
        |> List.filter (\x -> x /= "")
        |> createDecoding


createEncoding x acc =
    let
        firstChar =
            Maybe.withDefault ' ' (head x) |> String.fromChar
    in
    acc
        ++ (if length x == 1 then
                ""
            else
                x |> length |> toString
           )
        ++ firstChar


encode : String -> String
encode sequence =
    -- Convert to List
    String.toList sequence
        -- group List items by consecutive letters
        |> groupSequence
        -- Print length of group followed by letter of group
        |> List.foldl createEncoding ""


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
