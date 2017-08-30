module RunLengthEncoding exposing (decode, encode, version)

import List exposing (head)


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

groupSequence : List Char -> List String
groupSequence chars = 
    case head chars of
        Just c ->
            
        Nothing ->
            