module Main exposing (..)

import Html exposing (..)
import TrenuvalochkaLib exposing (..)
import TrenuvalochkaSentences exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init firstSentence sentences
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
