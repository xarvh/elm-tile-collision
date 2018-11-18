module Viewport exposing (..)

import Browser.Dom
import Browser.Events
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import WebGL


-- Types


type alias Size length =
    { width : length
    , height : length
    }


type alias PixelSize =
    Size Int


type alias PixelPosition =
    { top : Int
    , left : Int
    }


type alias WorldPosition =
    { x : Float
    , y : Float
    }



-- Window getters


getWindowSize : (PixelSize -> msg) -> Cmd msg
getWindowSize msgConstructor =
    let
        viewportToMsg viewport =
            msgConstructor
                { width = floor viewport.viewport.width
                , height = floor viewport.viewport.height
                }
    in
    Task.perform viewportToMsg Browser.Dom.getViewport


onWindowResize : (PixelSize -> msg) -> Sub msg
onWindowResize msgConstructor =
    Browser.Events.onResize (\w h -> msgConstructor { width = w, height = h })



-- Normalized geometry


pixelToWorldUnits : PixelSize -> Float -> PixelPosition -> WorldPosition
pixelToWorldUnits pixelSize minimumContainedLength pixelPosition =
    let
        pixelX =
            pixelPosition.left - pixelSize.width // 2

        pixelY =
            1 - pixelPosition.top + pixelSize.height // 2

        { scaleX, scaleY } =
            worldToPixelScale pixelSize minimumContainedLength
    in
    { x = toFloat pixelX * minimumContainedLength / toFloat pixelSize.width / scaleX
    , y = toFloat pixelY * minimumContainedLength / toFloat pixelSize.height / scaleY
    }


worldToPixelScale : PixelSize -> Float -> { scaleX : Float, scaleY : Float }
worldToPixelScale pixelSize minimumContainedLength =
    let
        minSize =
            min pixelSize.width pixelSize.height
    in
    { scaleX = toFloat minSize / toFloat pixelSize.width
    , scaleY = toFloat minSize / toFloat pixelSize.height
    }


worldToPixelTransform : PixelSize -> Float -> Mat4
worldToPixelTransform pixelSize minimumContainedLength =
    let
        { scaleX, scaleY } =
            worldToPixelScale pixelSize minimumContainedLength
    in
    Mat4.makeScale (vec3 scaleX scaleY 1)



-- DOM element


toHtml : PixelSize -> List WebGL.Entity -> Html a
toHtml pixelSize entities =
    div
        [ style "width" (String.fromInt pixelSize.width ++ "px")
        , style "height" (String.fromInt pixelSize.height ++ "px")
        , style "overflow" "hidden"
        ]
        [ WebGL.toHtmlWith
            -- TODO get these from args
            [ WebGL.alpha True
            , WebGL.antialias
            ]
            [ style "width" (String.fromInt pixelSize.width ++ "px")
            , style "height" (String.fromInt pixelSize.height ++ "px")
            , Html.Attributes.width pixelSize.width
            , Html.Attributes.height pixelSize.height
            ]
            entities
        ]
