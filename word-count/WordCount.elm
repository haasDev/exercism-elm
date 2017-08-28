module WordCount exposing (..)

import Dict exposing (..)
import Regex exposing (Regex, regex, replace)
import String exposing (toLower)


wordCount phrase =
    let
        inc =
            Maybe.withDefault 0 >> (+) 1 >> Just

        punctuation =
            regex "[!&@$%^:,]+"

        words =
            phrase
                |> toLower
                |> replace Regex.All
                    punctuation
                    (\_ -> "")
                |> String.words
    in
    List.foldl (\word acc -> update word inc acc) Dict.empty words
