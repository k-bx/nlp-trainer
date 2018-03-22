module Main exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Entity =
    { id : Int
    , form : String
    , lemma : String
    , upos : String
    , xpos : String
    , feats : String
    , head : Int
    , deprel : List String
    , deps : List ( Int, String )
    , misc : String
    }


type alias Sentence =
    List Entity


type alias Model =
    { sentences : Array Sentence
    , currSent : Sentence
    }


firstSentence : List Entity
firstSentence =
    [ Entity 1
        "It"
        "it"
        "PRON"
        "PRP"
        "Case=Nom|Gender=Neut|Number=Sing|Person=3|PronType=Prs"
        4
        [ "nsubj" ]
        [ ( 4, "nsubj" ) ]
        ""
    , Entity 2
        "was"
        "be"
        "AUX"
        "VBD"
        "Mood=Ind|Number=Sing|Person=3|Tense=Past|VerbForm=Fin"
        4
        [ "cop" ]
        [ ( 4, "cop" ) ]
        ""
    , Entity 3
        "an"
        "a"
        "DET"
        "DT"
        "Definite=Ind|PronType=Art"
        4
        [ "det" ]
        [ ( 4, "det" ) ]
        ""
    , Entity 4
        "evolution"
        "evolution"
        "NOUN"
        "NN"
        "Number=Sing"
        0
        [ "root" ]
        [ ( 0, "root" ) ]
        "SpaceAfter=No"
    , Entity 5
        "."
        "."
        "PUNCT"
        "."
        "_"
        4
        [ "punct" ]
        [ ( 4, "punct" ) ]
        ""
    ]


sentences : List Sentence
sentences =
    [ firstSentence
    ]


init : ( Model, Cmd Msg )
init =
    ( Model
        (Array.fromList sentences)
        firstSentence
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | NewSentence Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewSentence (Random.int 0 (Array.length model.sentences - 1)) )

        NewSentence sentenceId ->
            case Array.get sentenceId model.sentences of
                Just s ->
                    ( { model | currSent = s }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


posOptions : List String
posOptions =
    [ "NOUN", "VERB" ]


posOption : String -> Html Msg
posOption pos =
    option [ value pos ] [ text pos ]


renderEntity : Entity -> Html Msg
renderEntity entity =
    div []
        [ text entity.form
        , select [] (List.map posOption posOptions)
        ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Roll ] [ text "Roll" ]
        , ul [] (List.map renderEntity model.currSent)
        ]
