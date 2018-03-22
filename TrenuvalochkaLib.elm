module TrenuvalochkaLib exposing (..)

-- MODEL

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Maybe exposing (withDefault)
import Random

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
    , posSelections : Dict Int String
    , gaveUp : Bool
    }


init : Sentence -> List Sentence -> ( Model, Cmd Msg )
init firstSentence sentences =
    ( Model
        (Array.fromList sentences)
        firstSentence
        Dict.empty
        False
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | GiveUp
    | NewSentence Int
    | SetPos Int String


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

        SetPos id val ->
            ( { model | posSelections = Dict.insert id val model.posSelections }, Cmd.none )

        GiveUp ->
            ( { model | gaveUp = True }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


posEmpty : String
posEmpty =
    "---"


posOptions : List String
posOptions =
    [ posEmpty
    , "ADJ"
    , "ADP"
    , "ADV"
    , "AUX"
    , "CCONJ"
    , "DET"
    , "INTJ"
    , "NOUN"
    , "NUM"
    , "PART"
    , "PRON"
    , "PROPN"
    , "PUNCT"
    , "SCONJ"
    , "SYM"
    , "VERB"
    , "X"
    ]


posOption : String -> Html Msg
posOption pos =
    option [ value pos ] [ text pos ]


renderEntity : Bool -> ( Entity, Maybe String ) -> Html Msg
renderEntity gaveUp ( entity, mPos ) =
    let
        ( color, message ) =
            case mPos of
                Nothing ->
                    ( "grey", "" )

                Just p ->
                    if p == entity.upos then
                        ( "green", "yup" )
                    else
                        ( "red", "nope" )

        divElems =
            [ text entity.form
            , select [ on "change" (Json.map (SetPos entity.id) targetValue) ] (List.map posOption posOptions)
            , div [ style [ ( "color", color ) ] ] [ text message ]
            ]

        answer =
            [ text entity.upos ]

        divElems2 =
            if gaveUp then
                List.append divElems answer
            else
                divElems
    in
    div [] divElems2


renderRelations : Model -> Html Msg
renderRelations model =
    text ""

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Roll ] [ text "Roll" ]
        , button [ onClick GiveUp ] [ text "I give up" ]
        , ul [] (List.map (renderEntity model.gaveUp) (List.map (\x -> ( x, Dict.get x.id model.posSelections )) model.currSent))
        , renderRelations model
        ]
