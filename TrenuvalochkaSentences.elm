module TrenuvalochkaSentences exposing (..)

import TrenuvalochkaLib exposing (..)

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
