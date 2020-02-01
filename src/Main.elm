module Main exposing (..)

import Browser exposing (Document)
import Dict exposing (Dict)
import Element exposing (Element, text)
import Element.Background
import Element.Border
import Element.Input



-- MODEL


type alias Model =
    { strings : Dict Int String
    , string : String
    , index : Int
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { index = 4
      , strings =
            Dict.fromList
                [ ( 0, "Hello" )
                , ( 1, "world" )
                , ( 2, "yolo" )
                , ( 3, "lolo" )
                ]
      , string = ""
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "List animation"
    , body =
        [ Element.layout [ Element.height Element.fill, Element.width Element.fill ] <|
            Element.column
                [ Element.centerX
                , Element.width Element.fill
                ]
                (stringForm model :: Dict.foldr (\k v a -> stringView k v :: a) [] model.strings)
        ]
    }


stringForm : Model -> Element Msg
stringForm model =
    Element.row [ Element.height <| Element.px 100 ]
        [ Element.Input.text []
            { onChange = ChangedString
            , text = model.string
            , placeholder = Just <| Element.Input.placeholder [] (Element.text "string")
            , label = Element.Input.labelHidden "string"
            }
        , Element.Input.button
            [ Element.Background.color (Element.rgb255 0 0 255)
            , Element.focused
                [ Element.Background.color (Element.rgb255 255 0 255) ]
            , Element.padding 20
            ]
            { onPress = Just <| ClickedAddString, label = Element.text "Add" }
        ]


stringView : Int -> String -> Element Msg
stringView key todo =
    Element.row
        [ Element.height <| Element.px 100
        , Element.width Element.fill
        , Element.Background.color (Element.rgb255 127 127 127)
        , Element.Border.solid
        , Element.Border.color (Element.rgb 0 255 0)
        , Element.Border.width 5
        ]
        [ Element.el [ Element.padding 20 ] (Element.text todo)
        , Element.Input.button
            [ Element.Background.color (Element.rgb255 0 0 255)
            , Element.focused
                [ Element.Background.color (Element.rgb255 255 0 255) ]
            , Element.padding 20
            ]
            { onPress = Just <| ClickedRemoveString key, label = Element.text "remove" }
        ]



-- UPDATE


type Msg
    = ClickedRemoveString Int
    | ChangedString String
    | ClickedAddString
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedRemoveString key ->
            ( { model | strings = Dict.remove key model.strings }, Cmd.none )

        ChangedString string ->
            ( { model | string = string }, Cmd.none )

        ClickedAddString ->
            if not <| String.isEmpty model.string then
                ( { model
                    | strings = Dict.insert model.index model.string model.strings
                    , index = model.index + 1
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
