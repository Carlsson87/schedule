module PieChart exposing (..)

import Html exposing (Html)
import List.Extra as List
import String.Interpolate exposing (interpolate)
import Svg exposing (..)
import Svg.Attributes as Attr


type alias Slice =
    { percent : Float
    , color : String
    }


view : List Slice -> Html msg
view slices =
    Html.div
        [ Attr.class "pieChart"
        ]
        [ Svg.svg
            [ Attr.viewBox "-1 -1 2 2"
            , Attr.class "u-block"
            , Attr.style "transform: rotate(-90deg)"
            ]
            (List.mapAccuml
                getPathData
                0
                slices
                |> Tuple.second
            )
        ]


getPathData : Float -> Slice -> ( Float, Svg msg )
getPathData cumulativePercent { percent, color } =
    let
        newCumulativePercent =
            percent + cumulativePercent

        ( startX, startY ) =
            getCoordinatesForPercent cumulativePercent

        ( endX, endY ) =
            getCoordinatesForPercent newCumulativePercent

        largeArcFlag =
            if percent > 0.5 then
                1

            else
                0
    in
    ( newCumulativePercent
    , path
        [ Attr.d
            (interpolate "M {0} {1} A 1 1 0 {2} 1 {3} {4} L 0 0"
                [ String.fromFloat startX
                , String.fromFloat startY
                , String.fromFloat largeArcFlag
                , String.fromFloat endX
                , String.fromFloat endY
                ]
            )
        , Attr.fill color
        ]
        []
    )


getCoordinatesForPercent : Float -> ( Float, Float )
getCoordinatesForPercent percent =
    let
        x =
            cos (2 * pi * percent)

        y =
            sin (2 * pi * percent)
    in
    ( x, y )
