module Main exposing (..)

import Browser
import Browser.Events
import Game
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode exposing (Decoder)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Scene
import TileCollision exposing (Vector)
import Time exposing (Posix)
import Viewport exposing (PixelPosition, PixelSize)
import WebGL


-- Types


type alias Flags =
    {}


type alias Model =
    { viewportSize : PixelSize
    , mousePosition : PixelPosition

    --, clickPosition : PixelPosition
    , currentTimeInSeconds : Float
    , position : Vec2
    , velocity : Vec2
    }


type Msg
    = OnResize PixelSize
    | OnMouseMove PixelPosition
    | OnMouseClick
    | OnAnimationFrame Float



-- Init


worldSize =
    10


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { viewportSize =
                { width = 640
                , height = 480
                }
            , mousePosition =
                { top = 320
                , left = 240
                }
            , position = vec2 0 0
            , velocity = vec2 0 0
            , currentTimeInSeconds = 0
            }

        cmd =
            Viewport.getWindowSize OnResize
    in
    ( model, cmd )



-- Update


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnResize size ->
            noCmd { model | viewportSize = size }

        OnMouseMove position ->
            noCmd { model | mousePosition = position }

        OnMouseClick ->
            let
                clickPosition =
                    Vec2.fromRecord <| Viewport.pixelToWorldUnits model.viewportSize worldSize model.mousePosition

                velocity =
                    Vec2.sub clickPosition model.position
                        |> Vec2.scale 0.5
            in
            --noCmd { model | velocity = velocity }
            noCmd { model | position = clickPosition }

        OnAnimationFrame dtInMilliseconds ->
            let
                dt =
                    dtInMilliseconds / 1000

                dp =
                    Vec2.scale dt model.velocity

                velocity =
                    Vec2.scale (0.98 ^ dt) model.velocity

                start =
                    model.position

                end =
                    Vec2.add model.position dp

                {-
                ( fixedPosition, maybeCollision ) =
                    TileCollision.collisions
                        { hasBlockerAlong = Game.hasBlockerAlong
                        , tileSize = Game.tileSize
                        , mobSize = Game.mobSize
                        , start = Game.vec2ints start
                        , end = Game.vec2ints end
                        }
                -}
            in
            noCmd
                { model
                    | currentTimeInSeconds = model.currentTimeInSeconds + dt / 1000
                    , velocity = velocity
                    --, position = Game.ints2vec fixedPosition
                }



-- View


view : Model -> Browser.Document Msg
view model =
    let
        entities =
            Scene.entities
                { cameraToViewport = Viewport.worldToPixelTransform model.viewportSize worldSize
                , mousePosition = Vec2.fromRecord <| Viewport.pixelToWorldUnits model.viewportSize worldSize model.mousePosition
                , clickPosition = model.position
                , time = model.currentTimeInSeconds
                }
    in
    { title = "WebGL Scaffold"
    , body =
        [ Viewport.toHtml model.viewportSize entities
        , Html.node "style" [] [ Html.text "body { margin: 0; }" ]
        ]
    }



-- Subscriptions


mousePositionDecoder : Decoder PixelPosition
mousePositionDecoder =
    Json.Decode.map2 (\x y -> { left = x, top = y })
        (Json.Decode.field "clientX" Json.Decode.int)
        (Json.Decode.field "clientY" Json.Decode.int)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Viewport.onWindowResize OnResize
        , Browser.Events.onAnimationFrameDelta OnAnimationFrame
        , Browser.Events.onMouseMove mousePositionDecoder |> Sub.map OnMouseMove
        , Browser.Events.onClick (Json.Decode.succeed OnMouseClick)
        ]



-- Main


main =
    Browser.document
        { view = view
        , subscriptions = subscriptions
        , update = update
        , init = init
        }
