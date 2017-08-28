module RNATranscription exposing (toRNA)

import String exposing (cons, toList)


toRNA : String -> Result Char String
toRNA dna =
    let
        transcribe : Char -> Result Char Char
        transcribe chr =
            case chr of
                'A' ->
                    Ok 'U'

                'C' ->
                    Ok 'G'

                'G' ->
                    Ok 'C'

                'T' ->
                    Ok 'A'

                _ ->
                    Err chr
    in
    case String.uncons dna of
        Just ( head, tail ) ->
            Result.map2 cons (transcribe head) (toRNA tail)

        Nothing ->
            Ok ""
