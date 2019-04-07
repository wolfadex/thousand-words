module Main exposing (main)

import Browser exposing (Document)
import Canvas exposing (Shape)
import Color
import Debug
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Regex exposing (Regex)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { text : String
    , words : Patterns
    , specialCharacters : Patterns
    }


type alias Patterns =
    { strings : List ( String, Int )
    , longest : Int
    , mostFrequent : Int
    }


type Msg
    = NoOp
    | ParseText
    | UpdateText String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { text = ""
      , words =
            { strings = []
            , longest = 0
            , mostFrequent = 0
            }
      , specialCharacters =
            { strings = []
            , longest = 0
            , mostFrequent = 0
            }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ text } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ParseText ->
            let
                scs =
                    toPatternStrings filterSpecialCharacters text

                bracketsFound =
                    scs
                        |> List.filter
                            (\( word, _ ) ->
                                List.member word brackets
                            )

                finalSCS =
                    scs
                        |> List.foldl
                            (\( word, count ) total ->
                                if word == "\"" then
                                    if modBy 2 count == 0 then
                                        ( "\"\"", count // 2 ) :: total

                                    else
                                        ( "\"\"", count // 2 ) :: ( word, 1 ) :: total

                                else if List.member word brackets then
                                    let
                                        bracketMatch =
                                            getMatchingBracket word

                                        matchCount =
                                            bracketsFound
                                                |> List.foldl
                                                    (\( maybeMatch, maybeCount ) res ->
                                                        if bracketMatch == maybeMatch then
                                                            maybeCount

                                                        else
                                                            res
                                                    )
                                                    0
                                    in
                                    if List.member word leftBrackets then
                                        if matchCount > 0 then
                                            if count > matchCount then
                                                ( word, count - matchCount ) :: ( word ++ bracketMatch, matchCount ) :: total

                                            else
                                                ( word ++ bracketMatch, count ) :: total

                                        else
                                            ( word, count ) :: total

                                    else if count > matchCount then
                                        ( word, count - matchCount ) :: total

                                    else
                                        total

                                else
                                    ( word, count ) :: total
                            )
                            []

                ( longestSC, mostFrequestSC ) =
                    getCounts finalSCS
            in
            ( { model
                | words = stringToPatterns (String.toLower >> filterToLatin) text
                , specialCharacters =
                    { strings = finalSCS
                    , longest = longestSC
                    , mostFrequent = mostFrequestSC
                    }

                -- stringToPatterns filterSpecialCharacters text
              }
            , Cmd.none
            )

        UpdateText nextText ->
            ( { model | text = nextText }, Cmd.none )


brackets : List String
brackets =
    leftBrackets ++ rightBrackets


rightBrackets : List String
rightBrackets =
    [ ")", "}", "]", ">" ]


leftBrackets : List String
leftBrackets =
    [ "(", "{", "[", "<" ]


getMatchingBracket : String -> String
getMatchingBracket bracket =
    case bracket of
        "(" ->
            ")"

        "{" ->
            "}"

        "[" ->
            "]"

        "<" ->
            ">"

        ")" ->
            "("

        "}" ->
            "{"

        "]" ->
            "["

        ">" ->
            "<"

        _ ->
            ""


stringToPatterns : (String -> String) -> String -> Patterns
stringToPatterns filter str =
    let
        strings =
            toPatternStrings filter str

        ( longest, mostFrequent ) =
            getCounts strings
    in
    { strings = strings
    , longest = longest
    , mostFrequent = mostFrequent
    }


toPatternStrings : (String -> String) -> String -> List ( String, Int )
toPatternStrings filter text =
    text
        |> String.words
        |> List.map filter
        |> groupStrings
        |> Dict.toList


groupStrings : List String -> Dict String Int
groupStrings =
    List.foldl
        (\word counts ->
            counts
                |> Dict.update word
                    (\maybeCount ->
                        if word == "" then
                            Nothing

                        else
                            case maybeCount of
                                Nothing ->
                                    Just 1

                                Just count ->
                                    Just <| count + 1
                    )
        )
        Dict.empty


getCounts : List ( String, Int ) -> ( Int, Int )
getCounts =
    List.foldl
        (\( word, count ) ( biggestSize, biggestCount ) ->
            ( max biggestSize <| String.length word
            , max biggestCount count
            )
        )
        ( 0, 0 )


onlyLatinRegex : Regex
onlyLatinRegex =
    Maybe.withDefault
        Regex.never
    <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = True
            }
            "[^a-z]"


filterToLatin : String -> String
filterToLatin =
    Regex.replace
        onlyLatinRegex
        (\_ -> "")


onlySpecialCharactersRegex : Regex
onlySpecialCharactersRegex =
    Maybe.withDefault
        Regex.never
    <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = True
            }
            "[a-zA-Z]"


filterSpecialCharacters : String -> String
filterSpecialCharacters =
    Regex.replace
        onlySpecialCharactersRegex
        (\_ -> "")


view : Model -> Document Msg
view { text, words, specialCharacters } =
    { title = "Thousand Words"
    , body =
        [ Html.text "- Circles are words"
        , Html.br [] []
        , Html.text "- Squares are special characters: !, @, #, etc"
        , Html.br [] []
        , Html.text "- The X position is the relative length of the word"
        , Html.br [] []
        , Html.text "- The Y position is the relative frequency of the word"
        , Html.br [] []
        , Html.text "- The hue is the sum of character codes, modulus 360"
        , Html.br [] []
        , Html.text "- The size is the length of the word"
        , Html.form
            [ Events.onSubmit ParseText ]
            [ Html.textarea
                [ Attrs.value text
                , Events.onInput UpdateText
                , Attrs.placeholder "Your Text"
                ]
                []
            , Html.button
                []
                [ Html.text "Colorify" ]
            ]
        , Canvas.toHtml ( 300, 300 )
            [ Attrs.style "border" "1px solid black" ]
            (Canvas.shapes [ Canvas.fill Color.white ] [ Canvas.rect ( 0, 0 ) 300 300 ]
                :: patternsToRects specialCharacters
                ++ patternsToCircles words
            )
        ]
    }


patternsToCircles : Patterns -> List Canvas.Renderable
patternsToCircles { strings, longest, mostFrequent } =
    strings
        |> List.map
            (\( str, count ) ->
                let
                    x =
                        (String.length str |> toFloat) / toFloat longest * 300

                    y =
                        (count |> toFloat) / (mostFrequent |> toFloat) * 300

                    hue =
                        (str
                            |> String.foldl (\char total -> Char.toCode char + total) 0
                            |> modBy 360
                            |> toFloat
                        )
                            / 360
                in
                Canvas.shapes
                    [ Canvas.fill <|
                        Color.hsla hue 1 0.5 0.5
                    ]
                    [ Canvas.circle ( x, y ) (toFloat count)
                    ]
            )


patternsToRects : Patterns -> List Canvas.Renderable
patternsToRects { strings, longest, mostFrequent } =
    strings
        |> List.map
            (\( str, count ) ->
                let
                    size =
                        toFloat count

                    x =
                        (String.length str |> toFloat) / toFloat longest * 300 - size / 2

                    y =
                        (count |> toFloat) / (mostFrequent |> toFloat) * 300 - size / 2

                    hue =
                        (str
                            |> String.foldl (\char total -> Char.toCode char + total) 0
                            |> modBy 360
                            |> toFloat
                        )
                            / 360
                in
                Canvas.shapes
                    [ Canvas.fill <|
                        Color.hsla hue 1 0.5 0.5
                    ]
                    [ Canvas.rect ( x, y ) size size
                    ]
            )
