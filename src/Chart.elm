module Chart exposing (Curve, Graph, circle, dataPoints, drawCurve, h, padding, preparedPoints, renderChart, w, xGridLine, xScale, yGridLine, yScale)

import Axis
import Color exposing (Color)
import Html exposing (Html, a, button, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Scale.Color
import Shape
import SubPath exposing (SubPath)
import TypedSvg exposing (g, line, rect, svg, text_)
import TypedSvg.Attributes as Explicit exposing (fill, fontFamily, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Fill(..), Transform(..), percent)



-- VIEW


w : Float
w =
    990


h : Float
h =
    450


padding : Float
padding =
    50


dataPoints : List ( Float, Float )
dataPoints =
    [ ( 8, 0.1 )
    , ( 9, 0.6 )
    , ( 10, 0.3 )
    , ( 11, 0.3 )
    , ( 12, 0.2 )
    , ( 13, 0.8 )
    , ( 14, 0.6 )
    , ( 15, 0.9 )
    , ( 16, 0.2 )
    , ( 17, 0.1 )
    , ( 18, 0.4 )
    , ( 19, 0.7 )
    , ( 20, 0.8 )
    ]


xScale : ContinuousScale Float
xScale =
    Scale.linear ( padding, w - padding ) ( 1, 20 )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - padding, padding ) ( 0, 1 )


preparedPoints : List ( Float, Float ) -> List ( Float, Float )
preparedPoints points =
    List.map (\( x, y ) -> ( Scale.convert xScale x, Scale.convert yScale y )) points


xGridLine : Int -> Float -> Svg msg
xGridLine index tick =
    line
        [ y1 0
        , Explicit.y2 (percent 100)
        , x1 (Scale.convert xScale tick)
        , x2 (Scale.convert xScale tick)
        , stroke Color.darkBlue
        , strokeWidth (Basics.max (toFloat (modBy 2 index)) 0.5)
        ]
        []


yGridLine : Int -> Float -> Svg msg
yGridLine index tick =
    line
        [ x1 0
        , Explicit.x2 (percent 100)
        , y1 (Scale.convert yScale tick)
        , y2 (Scale.convert yScale tick)
        , stroke Color.darkBlue
        , strokeWidth (Basics.max (toFloat (modBy 2 index)) 0.5)
        ]
        []


type alias Curve =
    List ( Float, Float ) -> SubPath



--( "Linear", basic "linearCurve" Shape.linearCurve )


drawCurve : Color -> List ( Float, Float ) -> Svg msg
drawCurve color points =
    List.map Just points
        |> Shape.line Shape.linearCurve
        |> (\path -> Path.element path [ stroke color, fill FillNone, strokeWidth 2 ])


type alias Graph =
    { dataPoints : List ( Float, Float )
    , color : Color
    }


renderChart : List Graph -> Html msg
renderChart graphs =
    let
        prep =
            preparedPoints
    in
    div [ style "width" "700px" ]
        [ svg [ viewBox 0 0 w h ]
            ([ --rect [ width w, height h, fill <| Fill <| Color.rgb255 223 223 223 ] []
               -- g [] <| List.indexedMap yGridLine <| Scale.ticks yScale 10
               --, g [] <| List.indexedMap xGridLine <| Scale.ticks xScale 20
               g []
                [ Axis.bottom [ Axis.tickCount 20 ] xScale ]
             , g []
                [ Axis.left [ Axis.tickCount 5 ] yScale ]

             --, g [] <| List.map (\( dx, dy ) -> Path.element circle [ fill (Fill Color.white), stroke Color.black, transform [ Translate dx dy ] ]) preparedPoints
             ]
                ++ (graphs |> List.map (\gr -> g [] [ drawCurve gr.color (preparedPoints gr.dataPoints) ]))
            )
        ]


circle : Path
circle =
    Shape.arc
        { innerRadius = 0
        , outerRadius = 3
        , cornerRadius = 0
        , startAngle = 0
        , endAngle = 2 * pi
        , padAngle = 0
        , padRadius = 0
        }
