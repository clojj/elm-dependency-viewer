module Dependencies exposing
    ( Dependency
    , parse
    )

import Browser
import Html exposing (Html, div, p, pre, text)
import Http
import Parser exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "http://127.0.0.1:8080/maven-snippet.txt"
        , expect = Http.expectString GotText
        }
    )


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success fullText, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book."

        Loading ->
            text "Loading..."

        Success txt ->
            case run dependency txt of
                Err err ->
                    text (Debug.toString err)

                Ok expr ->
                    div []
                        [ p [] [ text (Debug.toString expr) ]
                        ]



-- EXPRESSIONS


type Dependency
    = Dependency Gatvs
    | Transient Gatvs


type Gatvs
    = Gatvs Group Artefact Type Version Scope


type alias Group =
    String

type alias Artefact =
    String

type alias Version =
    String

type alias Type =
    String

type alias Scope =
    String


parse : String -> Result (List DeadEnd) Dependency
parse s =
    run dependency s



-- PARSER


dependency : Parser Dependency
dependency =
    succeed Dependency
        |. chompUntil "+-"
        |. token "+-"
        |. spaces
        |= parseGatvs


parseGatvs =
    succeed Gatvs
        |= var
        |. symbol ":"
        |= var
        |. symbol ":"
        |= var
        |. symbol ":"
        |= var
        |. symbol ":"
        |= var

var : Parser String
var =
  getChompedString <|
    succeed ()
      |. chompIf isStartChar
      |. chompWhile isInnerChar

isStartChar : Char -> Bool
isStartChar char =
  Char.isAlpha char || Char.isDigit char

isInnerChar : Char -> Bool
isInnerChar char =
  isStartChar char || Char.isDigit char || char == '.' || char == '-'
  
mavenPrefix =
    keyword "[INFO] "


mavenStartModule =
    keyword "--- maven-dependency-plugin"
