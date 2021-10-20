port module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as List
import PieChart


port updatedSchedule : String -> Cmd msg


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pointerIsDown then
        Browser.Events.onMouseUp (Decode.succeed MouseUp)

    else
        Sub.none


decodeModifierKey : Decoder ()
decodeModifierKey =
    Decode.field "metaKey" Decode.bool
        |> Decode.andThen
            (\key ->
                if key then
                    Decode.succeed ()

                else
                    Decode.fail ""
            )


type alias Model =
    { segments : Dict Int Activity
    , pointerIsDown : Bool
    , selectedActivity : Maybe Activity
    }


type Activity
    = Work
    | Meeting
    | CodeReview
    | Cooperation
    | SecondLine


init : String -> ( Model, Cmd Msg )
init flags =
    ( { segments = fromString <| String.dropLeft 3 flags
      , pointerIsDown = False
      , selectedActivity = Just Work
      }
    , Cmd.none
    )


type Msg
    = MouseDown Int
    | MouseMove Int
    | MouseUp
    | ActivityWasSelected (Maybe Activity)


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
            , updatedSchedule (export model.segments)
            )

        ActivityWasSelected activity ->
            ( { model | selectedActivity = activity }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        summary =
            activitySummary model.segments

        sumOfTime =
            List.map Tuple.second summary |> List.sum

        toSlice ( activity, time ) =
            { percent = toFloat time / toFloat sumOfTime
            , color = activityColor activity
            }
    in
    Html.div
        [ Attr.style "padding" "20px" ]
        [ Html.div
            [ Attr.style "display" "flex"
            ]
            [ Html.div
                [ Attr.style "flex" "0 0 auto"
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                ]
                (List.map (viewActivitySelect model.selectedActivity) [ Just Work, Just Meeting, Just CodeReview, Just Cooperation, Just SecondLine, Nothing ])
            , Html.div
                [ Attr.style "flex" "0 0 auto"
                , Attr.style "width" "200px"
                , Attr.style "margin-left" "40px"
                , Attr.style "padding-right" "54px"
                , Attr.style "user-select" "none"
                ]
                (List.range 0 48 |> List.map (viewSegment model.segments))
            , Html.div
                [ Attr.style "flex" "0 0 auto"
                , Attr.style "margin-left" "40px"
                ]
                [ Html.div
                    [ Attr.style "width" "200px"
                    , Attr.style "margin-bottom" "50px"
                    ]
                    [ case List.map toSlice summary of
                        [] ->
                            PieChart.view [ { percent = 1, color = "#eee" } ]

                        slices ->
                            PieChart.view slices
                    ]
                , Html.div
                    []
                    (List.map (viewSummary sumOfTime) (List.sortBy (Tuple.second >> negate) summary))
                , case List.sum (List.map Tuple.second summary) of
                    0 ->
                        Html.text ""

                    n ->
                        viewTotal (segmentsToHoursAndMinutes n)
                ]
            ]
        ]


viewTotal : ( Int, Int ) -> Html msg
viewTotal ( hh, mm ) =
    Html.div
        [ Attr.style "font-weight" "700"
        , Attr.style "padding-left" "32px"
        , Attr.style "border-top" "solid #CCC 2px"
        , Attr.style "margin-top" "4px"
        , Attr.style "padding-top" "4px"
        ]
        [ Html.text (String.fromInt hh ++ "h ")
        , Html.text (String.fromInt mm ++ "m")
        ]


viewActivitySelect : Maybe Activity -> Maybe Activity -> Html Msg
viewActivitySelect selectedActivity activity =
    Html.div
        [ Attr.style "margin-bottom" "25px"
        , Attr.style "text-align" "center"

        -- , Attr.style "font-weight" "700"
        , Attr.style "font-size" "14px"
        ]
        [ Html.div
            [ Attr.style "width" "50px"
            , Attr.style "height" "50px"
            , Attr.style "position" "relative"
            , Maybe.map activityColor activity
                |> Maybe.withDefault "#eee"
                |> Attr.style "background-color"
            , Attr.style "border-radius" "50px"
            , Attr.style "cursor" "pointer"
            , Attr.style "margin" "0 auto 4px auto"
            , Events.onClick (ActivityWasSelected activity)
            , if selectedActivity == activity then
                Attr.style "border" "5px solid black"

              else
                Attr.style "border" "5px solid transparent"
            ]
            []
        , Maybe.map activityString activity
            |> Maybe.withDefault "Inget"
            |> Html.text
        ]


segmentsToHoursAndMinutes : Int -> ( Int, Int )
segmentsToHoursAndMinutes n =
    ( n // 4, 15 * remainderBy 4 n )


viewSummary : Int -> ( Activity, Int ) -> Html msg
viewSummary sum ( activity, n ) =
    let
        hh =
            n // 4

        mm =
            15 * remainderBy 4 n

        percent =
            round (100 * (toFloat n / toFloat sum))
    in
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "margin-bottom" "4px"
        ]
        [ Html.div
            [ Attr.style "flex" "0 0 auto"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "margin-right" "12px"
            ]
            [ Html.div
                [ Attr.style "width" "20px"
                , Attr.style "height" "20px"
                , Attr.style "background-color" (activityColor activity)
                , Attr.style "border-radius" "50px"
                ]
                []
            ]
        , Html.text (String.fromInt hh ++ "h ")
        , Html.text (String.fromInt mm ++ "m")
        , Html.text " / "
        , Html.text (String.fromInt percent ++ "%")
        , Html.text " - "
        , Html.text (activityString activity)
        ]


viewSegment : Dict Int Activity -> Int -> Html Msg
viewSegment segments i =
    let
        activity =
            Dict.get i segments
    in
    Html.div
        [ Attr.style "height" "15px"
        , Maybe.map activityColor activity
            |> Maybe.withDefault "#eee"
            |> Attr.style "background-color"
        , Events.onMouseDown (MouseDown i)
        , Events.onMouseEnter (MouseMove i)
        , Attr.style "cursor" "crosshair"
        , Attr.style "position" "relative"
        , Attr.class "segment"
        , Attr.style "border-top" <|
            if modBy 4 i == 0 then
                "solid black 1px"

            else
                "solid transparent 1px"
        ]
        (case i of
            0 ->
                [ timeLabel "06:00" ]

            4 ->
                [ timeLabel "07:00" ]

            8 ->
                [ timeLabel "08:00" ]

            12 ->
                [ timeLabel "09:00" ]

            16 ->
                [ timeLabel "10:00" ]

            20 ->
                [ timeLabel "11:00" ]

            24 ->
                [ timeLabel "12:00" ]

            28 ->
                [ timeLabel "13:00" ]

            32 ->
                [ timeLabel "14:00" ]

            36 ->
                [ timeLabel "15:00" ]

            40 ->
                [ timeLabel "16:00" ]

            44 ->
                [ timeLabel "17:00" ]

            48 ->
                [ timeLabel "18:00" ]

            _ ->
                []
        )


timeLabel : String -> Html msg
timeLabel time =
    Html.span
        [ Attr.style "position" "absolute"
        , Attr.style "right" "-10px"
        , Attr.style "transform" "translate(100%, -50%)"
        , Attr.style "color" "#777"
        ]
        [ Html.text time ]


activitySummary : Dict Int Activity -> List ( Activity, Int )
activitySummary =
    let
        sumList ( ( first, activity ), rest ) =
            ( activity, 1 + List.length rest )
    in
    Dict.toList
        >> List.gatherEqualsBy Tuple.second
        >> List.map sumList


activityColor : Activity -> String
activityColor a =
    case a of
        Work ->
            "#baffc9"

        Meeting ->
            "#ffb3ba"

        CodeReview ->
            "#ffdfba"

        Cooperation ->
            "#bae1ff"

        SecondLine ->
            "#ffffba"


activityString : Activity -> String
activityString activity =
    case activity of
        Work ->
            "Jobb"

        Meeting ->
            "Möte"

        CodeReview ->
            "Granskning"

        Cooperation ->
            "Samarbete"

        SecondLine ->
            "2nd Line"


activityToInt : Activity -> Int
activityToInt activity =
    case activity of
        Work ->
            1

        Meeting ->
            2

        CodeReview ->
            3

        Cooperation ->
            4

        SecondLine ->
            5


intToActivity : Int -> Maybe Activity
intToActivity n =
    case n of
        1 ->
            Just Work

        2 ->
            Just Meeting

        3 ->
            Just CodeReview

        4 ->
            Just Cooperation

        5 ->
            Just SecondLine

        _ ->
            Nothing


simplify : List ( ( Int, Int ), Activity ) -> List ( ( Int, Int ), Activity )
simplify xs =
    case xs of
        [] ->
            xs

        _ :: [] ->
            xs

        (( ( start1, end1 ), act1 ) as head) :: (( ( start2, end2 ), act2 ) as neck) :: tail ->
            if act1 == act2 && (end1 + 1) == start2 then
                simplify (( ( start1, end2 ), act1 ) :: tail)

            else
                head :: simplify (neck :: tail)


export : Dict Int Activity -> String
export =
    let
        toString ( ( start, end ), activity ) =
            String.fromList [ toBase60 start, toBase60 end, toBase60 (activityToInt activity) ]

        encoder ( ( start, end ), activity ) =
            Encode.list Encode.int [ start, end, activityToInt activity ]
    in
    Dict.toList >> List.map (Tuple.mapFirst (\x -> ( x, x ))) >> simplify >> List.map toString >> String.join ""


fromString : String -> Dict Int Activity
fromString =
    let
        foldHelp ints dict =
            case ints of
                [ start, end, activity ] ->
                    case intToActivity activity of
                        Just a ->
                            List.range start end
                                |> List.foldl (\n -> Dict.insert n a) dict

                        Nothing ->
                            dict

                _ ->
                    dict
    in
    String.toList >> List.map fromBase60 >> List.groupsOf 3 >> List.foldl foldHelp Dict.empty


toBase60 : Int -> Char
toBase60 n =
    if n + 48 <= 57 then
        Char.fromCode (n + 48)

    else if n + 55 <= 90 then
        Char.fromCode (n + 55)

    else
        Char.fromCode (n + 61)


fromBase60 : Char -> Int
fromBase60 c =
    let
        n =
            Char.toCode c
    in
    if n >= 48 && n <= 57 then
        n - 48

    else if n >= 65 && n <= 90 then
        n - 55

    else
        n - 61
