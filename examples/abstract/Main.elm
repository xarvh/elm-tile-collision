module Main exposing (..)

import Browser
import Browser.Events
import Decompose exposing (AbsoluteAabbTrajectory, Collision, SquareBlocker, Vec)
import Dict
import Game
import Html exposing (..)
import Html.Attributes exposing (class, style)
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
    , mousePosition : Vec2
    , selectedCollision : Int
    , currentTimeInSeconds : Float
    , position : Vec2
    , velocity : Vec2
    , collisions : List (Collision SquareBlocker)
    , trajectory : AbsoluteAabbTrajectory
    }


type Msg
    = OnResize PixelSize
    | OnMouseMove PixelPosition
    | OnMouseClick
    | OnAnimationFrame Float
    | OnKeyPress String



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
                vec2 0 0

            {-
               { top = 320
               , left = 240
               }
            -}
            , selectedCollision = 0
            , position = vec2 0 0
            , velocity = vec2 0 0
            , currentTimeInSeconds = 0
            , collisions = []
            , trajectory =
                { start = Vec 0 0
                , end = Vec 0 0
                , width = 1
                , height = 1
                , minimumDistance = 1
                }
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
            noCmd { model | mousePosition = Vec2.fromRecord <| Viewport.pixelToWorldUnits model.viewportSize worldSize position }

        OnMouseClick ->
            noCmd { model | position = model.mousePosition }

        OnKeyPress key ->
            noCmd <|
                case Debug.log "" key of
                    "+" ->
                        { model | selectedCollision = max 0 (model.selectedCollision + 1) }

                    "-" ->
                        { model | selectedCollision = max 0 (model.selectedCollision - 1) }

                    _ ->
                        model

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
                getCollider { row, column } =
                    case Dict.get ( column, row ) Game.tilemap of
                        Just '#' ->
                            Decompose.squareBlocker

                        _ ->
                            Decompose.emptyTile

                trajectory =
                    { start = Vec2.toRecord model.position
                    , end = Vec2.toRecord model.mousePosition
                    , width = 2 * toFloat Game.mobSize.halfWidth / toFloat Game.tileSize
                    , height = 2 * toFloat Game.mobSize.halfHeight / toFloat Game.tileSize
                    , minimumDistance = 0.01
                    }

                collisions =
                    Decompose.collide getCollider trajectory
            in
            noCmd
                { model
                    | currentTimeInSeconds = model.currentTimeInSeconds + dt / 1000
                    , velocity = velocity
                    , collisions = collisions
                    , trajectory = trajectory
                }



-- View


css =
    """

body { margin: 0; }

.overlay {
  position: absolute;
  top: 0;
}

"""


view : Model -> Browser.Document Msg
view model =
    let
        entities =
            Scene.entities
                { cameraToViewport = Viewport.worldToPixelTransform model.viewportSize worldSize
                , mousePosition = model.mousePosition
                , clickPosition = model.position
                , time = model.currentTimeInSeconds
                , collision = List.drop model.selectedCollision model.collisions |> List.head
                , trajectory = model.trajectory
                }
    in
    { title = "WebGL Scaffold"
    , body =
        [ Viewport.toHtml model.viewportSize entities
        , div
            [ class "overlay" ]
            [ div
                []
                [ text (String.fromInt model.selectedCollision) ]
            , model.collisions
                |> List.map (Debug.toString >> text)
                |> ul []
            ]
        , Html.node "style" [] [ Html.text css ]
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
        , Browser.Events.onKeyPress (Json.Decode.map OnKeyPress <| Json.Decode.field "key" Json.Decode.string)
        ]



-- Main


main =
    Browser.document
        { view = view
        , subscriptions = subscriptions
        , update = update
        , init = init
        }
