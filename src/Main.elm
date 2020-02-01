module Main exposing (..)

import Animation
import Animation.Messenger
import Browser exposing (Document)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input



-- MODEL


type alias Model =
    { strings : Dict Int StringItem
    , string : String
    , index : Int
    }


type alias StringItem =
    { label : String
    , style : Animation.Messenger.State Msg
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { index = 4
      , strings =
            Dict.fromList
                [ ( 0, { label = "Hello", style = Animation.style animation100Percent } )
                , ( 1, { label = "world", style = Animation.style animation100Percent } )
                , ( 2, { label = "yolo", style = Animation.style animation100Percent } )
                , ( 3, { label = "lolo", style = Animation.style animation100Percent } )
                ]
      , string = ""
      }
    , Cmd.none
    )


animation100Percent =
    [ Animation.height (Animation.px 100)
    , Animation.opacity 1
    , Animation.scale 1
    ]


animation0Percent =
    [ Animation.height (Animation.px 0)
    , Animation.opacity 0
    , Animation.scale 0
    ]



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
                (stringInput model :: Dict.foldr (\k v a -> stringView k v :: a) [] model.strings)
        ]
    }


stringInput : Model -> Element Msg
stringInput model =
    Element.row [ Element.height <| Element.px 100 ]
        [ Element.Input.text
            [ Element.height (Element.px 60)
            , Element.padding 20
            , Element.Font.size 20
            ]
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


stringView : Int -> StringItem -> Element Msg
stringView key stringItem =
    Element.el
        ([ Element.height <| Element.px 100, Element.width Element.fill ]
            ++ (Animation.render stringItem.style
                    |> List.map Element.htmlAttribute
               )
        )
        (Element.row
            [ Element.height <| Element.px 100
            , Element.width Element.fill
            , Element.Background.color (Element.rgb255 127 127 127)
            , Element.Border.solid
            , Element.Border.color (Element.rgb 0 255 0)
            , Element.Border.width 5
            ]
            [ Element.el [ Element.padding 20 ] (Element.text stringItem.label)
            , Element.Input.button
                [ Element.Background.color (Element.rgb255 0 0 255)
                , Element.focused
                    [ Element.Background.color (Element.rgb255 255 0 255) ]
                , Element.padding 20
                ]
                { onPress = Just <| ClickedRemoveString key, label = Element.text "remove" }
            ]
        )



-- UPDATE


type Msg
    = ClickedRemoveString Int
    | ChangedString String
    | ClickedAddString
    | RemovedString Int
    | Animate Int Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedRemoveString key ->
            let
                animation item =
                    Animation.interrupt
                        [ Animation.to animation0Percent
                        , Animation.Messenger.send (RemovedString key)
                        ]
                        item.style

                strings =
                    Dict.update key (Maybe.map (\a -> { a | style = animation a })) model.strings
            in
            ( { model | strings = strings }, Cmd.none )

        RemovedString key ->
            ( { model | strings = Dict.remove key model.strings }, Cmd.none )

        ChangedString string ->
            ( { model | string = string }, Cmd.none )

        ClickedAddString ->
            if not <| String.isEmpty model.string then
                ( { model
                    | strings =
                        Dict.insert model.index
                            { label = model.string
                            , style =
                                Animation.interrupt [ Animation.to animation100Percent ]
                                    (Animation.style animation0Percent)
                            }
                            model.strings
                    , index = model.index + 1
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Animate key animMsg ->
            case Dict.get key model.strings of
                Just item ->
                    Animation.Messenger.update animMsg item.style
                        |> Tuple.mapFirst (\a -> { item | style = a })
                        |> Tuple.mapFirst (\a -> Dict.insert key a model.strings)
                        |> Tuple.mapFirst (\a -> { model | strings = a })

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (Dict.foldr
            (\k { style } a ->
                Animation.subscription (Animate k) [ style ] :: a
            )
            []
            model.strings
        )



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
