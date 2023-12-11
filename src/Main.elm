port module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Task
import Time


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port saveThings : Value -> Cmd msg


port copyText : String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.pointerIsDown then
            Browser.Events.onMouseUp (Decode.succeed MouseUp)

          else
            Sub.none
        , Time.every 1000 SecondTick
        ]


type Msg
    = MouseDown ( Int, Int )
    | MouseMove ( Int, Int )
    | MouseUp
    | ActivityWasSelected String
    | EditButtonWasClicked
    | ActivitiesWasUdpdated String
    | SaveButtonWasClicked
    | ClearScheduleWasClicked
    | SecondTick Time.Posix
    | GotTimezone Time.Zone
    | SummaryWasClicked String


type alias Model =
    { segments : Dict ( Int, Int ) String
    , activities : Dict String String
    , pointerIsDown : Bool
    , selectedActivity : Maybe String
    , mode : Mode
    , currentSegment : ( Int, Int )
    , timezone : Time.Zone
    }


encodeSchedule : Dict ( Int, Int ) String -> Value
encodeSchedule =
    Dict.toList
        >> Encode.list
            (\( ( h, s ), color ) ->
                Encode.list identity
                    [ Encode.int h, Encode.int s, Encode.string color ]
            )


encodeActivities : Dict String String -> Value
encodeActivities =
    Encode.dict identity Encode.string


encodeStuff : Model -> Value
encodeStuff { segments, activities } =
    Encode.object
        [ ( "segments", encodeSchedule segments )
        , ( "activities", encodeActivities activities )
        ]


stuffDecoder : Decoder ( Dict ( Int, Int ) String, Dict String String )
stuffDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "segments"
            (Decode.map Dict.fromList
                (Decode.list
                    (Decode.map2 Tuple.pair
                        (Decode.map2 Tuple.pair (Decode.index 0 Decode.int) (Decode.index 1 Decode.int))
                        (Decode.index 2 Decode.string)
                    )
                )
            )
        )
        (Decode.field "activities" (Decode.dict Decode.string))


type Mode
    = Schedule
    | EditingActivities String


defaultActivities : List ( String, String )
defaultActivities =
    [ ( "#689D6A", "Work" )
    , ( "#CC241D", "Meeting" )
    , ( "#D79921", "CodeReview" )
    , ( "#458588", "Cooperation" )
    , ( "#B16286", "SecondLine" )
    , ( "#D65D0E", "Misc" )
    ]


activitiesToString : Dict String String -> String
activitiesToString =
    Dict.toList
        >> List.map (\( color, name ) -> color ++ "=" ++ name)
        >> String.join "\n"


stringToActivities : String -> List ( String, String )
stringToActivities =
    let
        parse parts =
            case parts of
                [ color, name ] ->
                    Just ( color, name )

                _ ->
                    Nothing
    in
    String.lines
        >> List.filterMap (String.split "=" >> parse)


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        ( segments, activities ) =
            Decode.decodeValue stuffDecoder flags
                |> Result.withDefault ( Dict.empty, Dict.fromList defaultActivities )
    in
    ( { segments = segments
      , activities = activities
      , pointerIsDown = False
      , selectedActivity = Nothing
      , mode = Schedule
      , timezone = Time.utc
      , currentSegment = ( 0, 0 )
      }
    , Task.perform GotTimezone Time.here
    )


save : ( Model, Cmd msg ) -> ( Model, Cmd msg )
save ( model, cmd1 ) =
    ( model
    , Cmd.batch
        [ cmd1
        , saveThings
            (Encode.object
                [ ( "name", Encode.string "stuff" )
                , ( "content", encodeStuff model )
                ]
            )
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown y ->
            case model.selectedActivity of
                Nothing ->
                    ( { model | pointerIsDown = True, segments = Dict.remove y model.segments }
                    , Cmd.none
                    )

                Just activity ->
                    ( { model | pointerIsDown = True, segments = Dict.insert y activity model.segments }
                    , Cmd.none
                    )

        MouseMove y ->
            case ( model.pointerIsDown, model.selectedActivity ) of
                ( True, Nothing ) ->
                    ( { model | segments = Dict.remove y model.segments }
                    , Cmd.none
                    )

                ( True, Just activity ) ->
                    ( { model | segments = Dict.insert y activity model.segments }
                    , Cmd.none
                    )

                ( False, _ ) ->
                    ( model
                    , Cmd.none
                    )

        MouseUp ->
            ( { model | pointerIsDown = False }
            , Cmd.none
            )
                |> save

        ActivityWasSelected activity ->
            ( { model
                | selectedActivity =
                    if Just activity == model.selectedActivity then
                        Nothing

                    else
                        Just activity
              }
            , Cmd.none
            )

        ActivitiesWasUdpdated str ->
            case model.mode of
                Schedule ->
                    ( model, Cmd.none )

                EditingActivities _ ->
                    ( { model
                        | mode = EditingActivities str
                        , activities =
                            stringToActivities str
                                |> Dict.fromList
                      }
                    , Cmd.none
                    )

        EditButtonWasClicked ->
            ( { model | mode = EditingActivities (activitiesToString model.activities) }
            , Cmd.none
            )

        SaveButtonWasClicked ->
            ( { model | mode = Schedule }
            , Cmd.none
            )
                |> save

        ClearScheduleWasClicked ->
            ( { model | segments = Dict.empty }
            , Cmd.none
            )
                |> save

        GotTimezone zone ->
            ( { model | timezone = zone }
            , Cmd.none
            )

        SecondTick posix ->
            let
                currentSegment =
                    ( Time.toHour model.timezone posix
                    , Time.toMinute model.timezone posix // 10
                    )
            in
            ( { model | currentSegment = currentSegment }
            , Cmd.none
            )

        SummaryWasClicked str ->
            ( model
            , copyText str
            )


view : Model -> Html Msg
view model =
    let
        hourChunks =
            List.range 0 5

        viewHour h =
            Html.div
                [ Attr.class "relative mb-[2px]"
                ]
                [ Html.div
                    [ Attr.class "flex"
                    ]
                    (List.map (viewSegment h) hourChunks)
                , Html.div [ Attr.class "absolute top-0 left-0 -translate-x-full pr-4" ]
                    [ Html.text (String.fromInt h)
                    ]
                ]

        viewSegment h s =
            Html.div
                [ Attr.class "h-[24px] w-[24px] rounded mr-[2px] cursor-pointer hover:opacity-50"
                , Attr.classList
                    [ ( "animate-pulse", ( h, s ) == model.currentSegment ) ]
                , Events.onMouseDown (MouseDown ( h, s ))
                , Events.onMouseEnter (MouseMove ( h, s ))
                , Dict.get ( h, s ) model.segments
                    |> Maybe.withDefault "#464646"
                    |> Attr.style "background-color"
                ]
                []
    in
    Html.div
        [ Attr.class "flex items-center justify-center min-h-screen w-screen"
        , Attr.style "background-color" "#282828"
        , Attr.style "color" "#5A5A5A"
        , Attr.class "select-none"
        ]
        [ case model.mode of
            Schedule ->
                Html.div
                    []
                    (List.map viewHour (List.range 0 23))

            EditingActivities value ->
                Html.div
                    []
                    [ Html.textarea
                        [ Attr.value value
                        , Attr.rows 8
                        , Attr.class "min-w-[160px] p-4 rounded "
                        , Attr.style "background-color" "#5A5A5A"
                        , Attr.style "color" "#282828"
                        , Events.onInput ActivitiesWasUdpdated
                        ]
                        []
                    ]
        , Html.div [ Attr.class "pl-8" ]
            [ Html.ul []
                (List.map (viewActivityItem model.selectedActivity) (Dict.toList model.activities))
            , case model.mode of
                Schedule ->
                    Html.div
                        []
                        [ Html.button
                            [ Events.onClick EditButtonWasClicked
                            , Attr.class "underline p-2"
                            ]
                            [ Html.text "Edit"
                            ]
                        , Html.button
                            [ Events.onClick ClearScheduleWasClicked
                            , Attr.class "underline p-2"
                            ]
                            [ Html.text "Clear"
                            ]
                        ]

                EditingActivities _ ->
                    Html.button
                        [ Events.onClick SaveButtonWasClicked
                        , Attr.class "underline p-2"
                        ]
                        [ Html.text "Spara"
                        ]
            ]
        , countSegments model.segments
            |> toSummary model
            |> viewSummary
        ]


viewSummary : String -> Html Msg
viewSummary text =
    if text == "" then
        Html.text ""

    else
        Html.pre
            [ Attr.class "absolute bottom-4 left-4 cursor-pointer"
            , Events.onClick (SummaryWasClicked text)
            ]
            [ Html.text text
            , Html.p
                [ Attr.class "absolute -top-4 left-0 text-xs"
                ]
                [ Html.text "(click to copy)" ]
            ]


toSummary : Model -> List ( String, Int ) -> String
toSummary { activities } xs =
    let
        total =
            List.map Tuple.second xs
                |> List.sum

        toTime nrOfBlocks =
            String.fromInt (nrOfBlocks // 6) ++ "h" ++ String.fromInt (remainderBy 6 nrOfBlocks * 10) ++ "m"

        toPct nrOfBlocks =
            100
                * (toFloat nrOfBlocks / toFloat total)
                |> round
                |> String.fromInt

        f ( color, nrOfBlocks ) =
            case Dict.get color activities of
                Just activity ->
                    activity ++ " " ++ toPct nrOfBlocks ++ "% " ++ toTime nrOfBlocks

                Nothing ->
                    "??? " ++ toPct nrOfBlocks ++ "% " ++ toTime nrOfBlocks
    in
    List.map f xs
        |> String.join "\n"


viewActivityItem : Maybe String -> ( String, String ) -> Html Msg
viewActivityItem selected ( bgColor, activity ) =
    Html.li
        [ Attr.class "flex items-center p-2 rounded cursor-pointer"
        , Events.onClick (ActivityWasSelected bgColor)
        , if selected == Just bgColor then
            Attr.style "background-color" "#1D2021"

          else
            Attr.style "background-color" "transparent"
        ]
        [ Html.span
            [ Attr.class "h-[24px] w-[24px] rounded mr-[8px] inline-block"
            , Attr.style "background-color" bgColor
            ]
            []
        , Html.text activity
        ]


countSegments : Dict ( Int, Int ) String -> List ( String, Int )
countSegments =
    Dict.values >> List.frequencies
