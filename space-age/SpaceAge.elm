module SpaceAge exposing (..)


type Planet
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune


ageOn : Planet -> Int -> Float
ageOn planet seconds =
    let
        earthYear : Float
        earthYear =
            31557600

        ageOn_ : Int -> Float -> Float
        ageOn_ secs ratio =
            toFloat secs / ratio / earthYear
    in
    case planet of
        Earth ->
            ageOn_ seconds 1.0

        Mercury ->
            ageOn_ seconds 0.2408467

        Venus ->
            ageOn_ seconds 0.61519726

        Mars ->
            ageOn_ seconds 1.8808158

        Jupiter ->
            ageOn_ seconds 11.862615

        Saturn ->
            ageOn_ seconds 29.447498

        Uranus ->
            ageOn_ seconds 84.016846

        Neptune ->
            ageOn_ seconds 164.79132
