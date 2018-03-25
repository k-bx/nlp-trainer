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
    , deprel : String
    , deps : List ( Int, String )
    , misc : String
    }


type alias Sentence =
    List Entity


type alias RelSelection =
    { id : Int
    , from : Int
    , to : Int
    , rel : String
    }


type alias Model =
    { sentences : Array Sentence
    , currSent : Sentence
    , posSelections : Dict Int String
    , relSelections : List RelSelection
    , gaveUp : Bool
    }


init : Sentence -> List Sentence -> ( Model, Cmd Msg )
init firstSentence sentences =
    ( Model
        (Array.fromList sentences)
        firstSentence
        Dict.empty
        [ RelSelection 1 0 0 "---" ]
        -- [ RelSelection 1 4 3 "det" ]
        False
    , Cmd.none
    )



-- UPDATE


type Msg
    = Roll
    | GiveUp
    | NewSentence Int
    | SetPos Int String
    | SetFromWord Int String
    | SetToWord Int String
    | SetRel Int String
    | AddRelationGuess


getWordId : String -> Model -> Maybe Int
getWordId word model =
    if word == "(root)" then
        Just 0
    else
        List.head (List.map .id (List.filter (\x -> x.form == word) model.currSent))


setFromWordRel : Int -> Int -> RelSelection -> RelSelection
setFromWordRel relSelId wordId relSelection =
    if relSelection.id == relSelId then
        { relSelection | from = wordId }
    else
        relSelection


setToWordRel : Int -> Int -> RelSelection -> RelSelection
setToWordRel relSelId wordId relSelection =
    if relSelection.id == relSelId then
        { relSelection | to = wordId }
    else
        relSelection


setRelRel : Int -> String -> RelSelection -> RelSelection
setRelRel relSelId rel relSelection =
    if relSelection.id == relSelId then
        { relSelection | rel = rel }
    else
        relSelection


setFromWord : Int -> Model -> String -> Model
setFromWord relSelId model word =
    let
        mwordId =
            getWordId word model
    in
    Maybe.withDefault model (Maybe.map (\wordId -> { model | relSelections = List.map (setFromWordRel relSelId wordId) model.relSelections }) mwordId)


setToWord : Int -> Model -> String -> Model
setToWord relSelId model word =
    let
        mwordId =
            getWordId word model
    in
    Maybe.withDefault model (Maybe.map (\wordId -> { model | relSelections = List.map (setToWordRel relSelId wordId) model.relSelections }) mwordId)


setRel : Int -> Model -> String -> Model
setRel relSelId model rel =
    { model | relSelections = List.map (setRelRel relSelId rel) model.relSelections }


genRand : Model -> Cmd Msg
genRand model =
    Random.generate NewSentence
        (Random.int 0
            (Array.length model.sentences - 1)
        )


newSent : Int -> Model -> Model
newSent sentenceId model =
    case Array.get sentenceId model.sentences of
        Just s ->
            { model | currSent = s }

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, genRand model )

        NewSentence sentenceId ->
            ( newSent sentenceId model, Cmd.none )

        SetPos id val ->
            ( { model | posSelections = Dict.insert id val model.posSelections }, Cmd.none )

        GiveUp ->
            ( { model | gaveUp = True }, Cmd.none )

        SetFromWord relSelId word ->
            ( setFromWord relSelId model word, Cmd.none )

        SetToWord relSelId word ->
            ( setToWord relSelId model word, Cmd.none )

        SetRel relSelId rel ->
            ( setRel relSelId model rel, Cmd.none )

        AddRelationGuess ->
            ( model, Cmd.none )



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


getHead : Model -> Int -> Maybe Entity
getHead model id =
    List.head (List.filter (\x -> x.id == id) model.currSent)


renderRelationEntityAnswer : Model -> Entity -> Html Msg
renderRelationEntityAnswer model entity =
    let
        head =
            getHead model entity.head

        els =
            [ text (withDefault "root" (Maybe.map .form head))
            , text "->"
            , text entity.deprel
            , text "->"
            , text entity.form
            ]
    in
    ul []
        (if model.gaveUp then
            els
         else
            []
        )


relOptions : List String
relOptions =
    [ "---"
    , "acl"
    , "advcl"
    , "advmod"
    , "amod"
    , "appos"
    , "aux"
    , "case"
    , "cc"
    , "ccomp"
    , "clf"
    , "compound"
    , "conj"
    , "cop"
    , "csubj"
    , "dep"
    , "det"
    , "discourse"
    , "dislocated"
    , "expl"
    , "fixed"
    , "flat"
    , "goeswith"
    , "iobj"
    , "list"
    , "mark"
    , "nmod"
    , "nsubj"
    , "nummod"
    , "obj"
    , "obl"
    , "orphan"
    , "parataxis"
    , "punct"
    , "reparandum"
    , "root"
    , "vocative"
    , "xcomp"
    ]


relOption : String -> Html Msg
relOption rel =
    option [ value rel ] [ text rel ]


getWords : Sentence -> List String
getWords sentence =
    List.map .form sentence


relWordSelection : String -> Html Msg
relWordSelection word =
    option [ value word ] [ text word ]


renderWordSelection : (Int -> String -> Msg) -> RelSelection -> Model -> Html Msg
renderWordSelection ev relSelection model =
    select [ on "change" (Json.map (ev relSelection.id) targetValue) ] (List.map relWordSelection ("---" :: "(root)" :: getWords model.currSent))


findRel : Sentence -> RelSelection -> Bool
findRel sent rs =
    let
        f x =
            x.head == rs.from && x.id == rs.to && x.deprel == rs.rel
    in
    Maybe.withDefault False (Maybe.map (const True) (List.head (List.filter f sent)))


renderRelationEntityGuess : Model -> RelSelection -> Html Msg
renderRelationEntityGuess model relSelection =
    let
        ( color, message ) =
            case findRel model.currSent relSelection of
                False ->
                    ( "red", "nada" )

                True ->
                    ( "green", "aha" )
    in
    ul []
        [ renderWordSelection SetFromWord relSelection model
        , select [ on "change" (Json.map (SetRel relSelection.id) targetValue) ] (List.map relOption relOptions)
        , renderWordSelection SetToWord relSelection model
        , span [ style [ ( "color", color ) ] ] [ text message ]
        ]


renderRelations : Model -> Html Msg
renderRelations model =
    let
        answers =
            List.map (renderRelationEntityAnswer model) model.currSent

        addNew =
            button [ onClick AddRelationGuess ] [ text "+" ]

        guesses =
            List.map (renderRelationEntityGuess model) model.relSelections
    in
    ul [] (List.append guesses (addNew :: answers))


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Welcome to Trenuvalochka" ]
        , button [ onClick Roll ] [ text "Roll" ]
        , button [ onClick GiveUp ] [ text "I give up" ]
        , ul [] (List.map (renderEntity model.gaveUp) (List.map (\x -> ( x, Dict.get x.id model.posSelections )) model.currSent))
        , renderRelations model
        ]


mkundefined : b -> a
mkundefined x =
    mkundefined x


const : a -> b -> a
const x y =
    x
