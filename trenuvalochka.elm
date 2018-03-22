module Main exposing (..)

import TrenuvalochkaSentences exposing (..)
import TrenuvalochkaLib exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init firstSentence sentences
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
