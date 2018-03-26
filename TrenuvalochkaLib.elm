module TrenuvalochkaLib exposing (..)

-- MODEL
--import Bootstrap.Dropdown as Dropdown

import Array exposing (Array)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Spacing as Spacing
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
    | RemoveRelationGuess Int


getWordId : String -> Model -> Maybe Int
getWordId word model =
    if word == "(root)" then
        Just 0
    else
        List.head (List.map .id (List.filter (\x -> String.toLower x.form == String.toLower word) model.currSent))


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


getNewRelSelectionId : Model -> Int
getNewRelSelectionId model =
    Maybe.withDefault 0 (List.maximum (List.map .id model.relSelections)) + 1


addRelGuess : Model -> Model
addRelGuess model =
    let
        newId =
            getNewRelSelectionId model
    in
    { model | relSelections = List.append model.relSelections [ RelSelection newId 0 0 "---" ] }


removeRelGuess : Int -> Model -> Model
removeRelGuess relSelId model =
    { model | relSelections = List.filter (\x -> x.id /= relSelId) model.relSelections }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( { model | gaveUp = False, relSelections = [], posSelections = Dict.empty }, genRand model )

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
            ( addRelGuess model, Cmd.none )

        RemoveRelationGuess relSelId ->
            ( removeRelGuess relSelId model, Cmd.none )



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


posOption : Maybe String -> String -> Html Msg
posOption mCurrSelection pos =
    let
        opts =
            if (mCurrSelection == Nothing && pos == posEmpty) || mCurrSelection == Just pos then
                [ selected True, value pos ]
            else
                [ value pos ]
    in
    option opts [ text pos ]


renderEntity : Model -> Bool -> ( Entity, Maybe String ) -> Html Msg
renderEntity model gaveUp ( entity, mPos ) =
    let
        message =
            case mPos of
                Nothing ->
                    div [] []

                Just p ->
                    if p == entity.upos then
                        Alert.simpleSuccess [] [ text "yup" ]
                    else
                        Alert.simpleDanger [] [ text "nope" ]

        divElems =
            [ div [] [ text entity.form ]
            , div [] [ select [ on "change" (Json.map (SetPos entity.id) targetValue) ] (List.map (posOption (Dict.get entity.id model.posSelections)) posOptions) ]
            , message
            ]

        answer =
            [ Alert.simpleInfo [] [ text entity.upos ]
            ]

        divElems2 =
            if gaveUp then
                List.append divElems answer
            else
                divElems
    in
    div [ style [ ( "float", "left" ) ] ] divElems2


getHead : Model -> Int -> Maybe Entity
getHead model id =
    List.head (List.filter (\x -> x.id == id) model.currSent)


renderRelationEntityAnswer : Model -> Entity -> Html Msg
renderRelationEntityAnswer model entity =
    let
        head =
            getHead model entity.head

        els =
            [ Alert.simpleInfo []
                [ text
                    (withDefault "root" (Maybe.map .form head)
                        ++ " ⟶ "
                        ++ entity.deprel
                        ++ " ⟶ "
                        ++ entity.form
                    )
                ]
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


relOption : String -> String -> Html Msg
relOption sel rel =
    let
        opts =
            if sel == rel then
                [ selected True, value rel ]
            else
                [ value rel ]
    in
    option opts [ text rel ]


getWords : Sentence -> List String
getWords sentence =
    List.map .form sentence


relWordSelection : String -> String -> Html Msg
relWordSelection currVal word =
    let
        opts =
            if currVal == word then
                [ selected True, value word ]
            else
                [ value word ]
    in
    option opts [ text word ]


renderWordSelection : (Int -> String -> Msg) -> RelSelection -> String -> Model -> Html Msg
renderWordSelection ev relSelection currVal model =
    select [ on "change" (Json.map (ev relSelection.id) targetValue) ]
        (List.map (relWordSelection currVal) ("---" :: "(root)" :: getWords model.currSent))


findRel : Sentence -> RelSelection -> Bool
findRel sent rs =
    let
        f x =
            x.head == rs.from && x.id == rs.to && String.toLower x.deprel == String.toLower rs.rel
    in
    Maybe.withDefault False (Maybe.map (const True) (List.head (List.filter f sent)))


findFromWord : Model -> Int -> String
findFromWord model fromId =
    Maybe.withDefault "" (List.head (List.map .form (List.filter (\x -> x.id == fromId) model.currSent)))


findToWord : Model -> Int -> String
findToWord model toId =
    Maybe.withDefault "" (List.head (List.map .form (List.filter (\x -> x.id == toId) model.currSent)))


renderRelationEntityGuess : Model -> RelSelection -> Html Msg
renderRelationEntityGuess model relSelection =
    let
        message =
            case findRel model.currSent relSelection of
                False ->
                    Alert.simpleDanger [] [ text "nada" ]

                True ->
                    Alert.simpleSuccess [] [ text "yeah" ]
    in
    ul []
        [ div [ style [ ( "float", "left" ) ] ] [ renderWordSelection SetFromWord relSelection (findFromWord model relSelection.from) model ]
        , div [ style [ ( "float", "left" ) ] ] [ select [ on "change" (Json.map (SetRel relSelection.id) targetValue) ] (List.map (relOption relSelection.rel) relOptions) ]
        , div [ style [ ( "float", "left" ) ] ] [ renderWordSelection SetToWord relSelection (findToWord model relSelection.to) model ]
        , div [ style [ ( "float", "left" ), ( "margin-right", "5px" ) ] ] [ Button.button [ Button.info, Button.attrs [ onClick (RemoveRelationGuess relSelection.id), Spacing.ml1 ] ] [ text "-" ] ]
        , div [ style [ ( "float", "left" ) ] ] [ message ]
        , div [ style [ ( "clear", "both" ) ] ] []
        ]


renderRelations : Model -> Html Msg
renderRelations model =
    let
        answers =
            List.map (renderRelationEntityAnswer model) model.currSent

        addNew =
            Button.button [ Button.info, Button.attrs [ onClick AddRelationGuess, Spacing.ml1 ] ] [ text "+" ]

        guesses =
            List.map (renderRelationEntityGuess model) model.relSelections
    in
    ul [] (List.append guesses (addNew :: answers))


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col []
                [ div
                    []
                    [ h1 [] [ text "Welcome to Trenuvalochka" ]
                    , Button.button [ Button.info, Button.attrs [ onClick Roll, Spacing.ml1 ] ] [ text "Roll" ]
                    , Button.button [ Button.info, Button.attrs [ onClick GiveUp, Spacing.ml1 ] ] [ text "I Give Up" ]
                    , div [ style [ ( "margin-bottom", "10px" ) ] ] []
                    , div [] [ text "Please, select the correct POS tags:" ]
                    , ul [] (List.map (renderEntity model model.gaveUp) (List.map (\x -> ( x, Dict.get x.id model.posSelections )) model.currSent))
                    , div [ style [ ( "clear", "both" ), ( "margin-bottom", "20px" ) ] ] []
                    , div [] [ text "Build a correct entity relations graph:" ]
                    , renderRelations model
                    ]
                ]
            ]
        ]


mkundefined : b -> a
mkundefined x =
    mkundefined x


const : a -> b -> a
const x y =
    x
