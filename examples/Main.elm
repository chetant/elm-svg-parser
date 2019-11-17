module Main exposing (main)

import Browser
import Svg.Parser exposing (..)
import Svg exposing (svg)
import Svg.Events exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html, button, div, text)
import Json.Decode as Decode
import Debug exposing (toString)
import Maybe exposing (andThen)
import Set
import Dict

main = Browser.element {
         init = init
       , update = update
       , subscriptions = subscriptions
       , view = view
       }

onClickStop : msg -> Svg.Attribute msg
onClickStop message = stopPropagationOn "click" (Decode.succeed (message, True))

onWheelZoom : Svg.Attribute Msg
onWheelZoom =
    let encodeZoom x = if x > 0 then (ZoomIn, True) else (ZoomOut, True)
    in stopPropagationOn "wheel"
        (Decode.field "deltaY" Decode.float |> Decode.map encodeZoom)

type alias Model =
    { svgModel : SvgNode Msg
    , scale : Int
    , selected : Set.Set NodeId
    , rightClicked : Maybe NodeId
    , states : Dict.Dict NodeId State
    }
type alias NodeId = String
type alias State = String
type alias Color = String

colorMap : Dict.Dict State Color
colorMap = Dict.fromList [
            ("Running",  "#66ff66")
           ,("Done",     "#b3b3cc")
           ,("Error",    "#ff4d4d")
           ,("Timeout",  "#9933ff")
           ]

init : () -> (Model, Cmd Msg)
init _ = ({ svgModel = case (parse testSvg) of
                           Err err -> SvgComment err
                           Ok val -> updSvgNode idAf (\_ -> setEvents >> setStrokeWidth) () (rescale 100 val)
          , scale = 100
          , selected = Set.empty
          , rightClicked = Nothing
          , states = Dict.fromList [
                      ("d", "Running")
                     ,("a", "Done")
                     ,("b", "Error")
                     ,("c", "Timeout")
                     ]
          }
         , Cmd.none
         )

setEvents e =
    if hasAttr matchNode e then
        case getNodeId e.attributes of
            Just nid ->
                { e | attributes =
                      (EventAttr <| onClickStop <| Clicked nid) ::
                      e.attributes
                      -- ((EventAttr <| onRightClick <| RightClicked nid) ::
                      --      e.attributes)
                }
            Nothing -> e
    else if hasAttr matchGraph e then
             { e | attributes =
                   (EventAttr <| onClickStop <| ClickedOutside) ::
                   ((EventAttr <| onWheelZoom) :: e.attributes)
             }
    else e

setStrokeWidth e =
    if hasAttr isStrokeAttr e then
        { e | attributes =
              (SimpleAttr "stroke-width" "1") ::
              e.attributes
        }
    else e

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

type Msg = Clicked NodeId
         -- | RightClicked NodeId
         | ClickedOutside
         | ZoomIn
         | ZoomOut

deltaZoom : Int
deltaZoom = 2

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Clicked nid -> ({ model | selected = Set.singleton nid }, Cmd.none)
      ClickedOutside -> ({ model | selected = Set.empty }, Cmd.none)
      ZoomIn -> ({ model | scale = model.scale - deltaZoom }, Cmd.none)
      ZoomOut -> ({ model | scale = model.scale + deltaZoom }, Cmd.none)

view : Model -> Html Msg
view model =
    -- text (toString model)
    div []
        [toSvg <| process model]

type alias NodeUpd =
    { model : Model
    , isSelected : Bool
    , nodeId : Maybe NodeId
    }
initNodeUpd m = { model = m, isSelected = False, nodeId = Nothing}
process model = rescale model.scale
                <| updSvgNode accf updf (initNodeUpd model) model.svgModel

updf x e = markSelected x e
         |> colorStates x

matchAttr an av a =
    case a of
        SimpleAttr bn bv -> (an == bn) && (av == bv)
        _ -> False

matchGraph = matchAttr "class" "graph"
matchNode = matchAttr "class" "node"
isAttrType t a =
    case a of
        SimpleAttr st _ -> (st == t)
        _ -> False
isFillAttr = isAttrType "fill"
isStrokeAttr = isAttrType "stroke"

hasAttr f e = List.any f e.attributes

getAttrId a =
    case a of
        SimpleAttr "id" n -> Just n
        _ -> Nothing

getNodeId attrs = List.filterMap getAttrId attrs |> List.head

accf x e =
    case getNodeId e.attributes of
        Just nid -> { x | isSelected = Set.member nid x.model.selected
                        , nodeId = Just nid
                    }
        Nothing -> x

setAttr ak av a =
    case a of
        SimpleAttr k _ -> if k == ak then SimpleAttr k av else a
        x -> a

setSel = setAttr "stroke-width" "4"
setFill c = setAttr "fill" c

markSelected x e =
    if x.isSelected
    then { e | attributes = List.map setSel e.attributes }
    else e

lkup m k = Dict.get k m

colorStates x e =
    if e.name /= "text" && hasAttr isFillAttr e then
        case (x.nodeId
             |> andThen (lkup x.model.states)
             |> andThen (lkup colorMap)) of
            Just col -> { e | attributes = List.map (setFill col) e.attributes }
            Nothing -> e
    else e

idAf x _ = x
idUf _ n = n

updSvgNode : (x -> Element a -> x) -> (x -> Element a -> Element b) -> x -> SvgNode a -> SvgNode b
updSvgNode af uf x n =
    case n of
        SvgElement e ->
            let
                y = af x e
                ns = List.map (updSvgNode af uf y) e.children
                m = uf x { e | children = [] }
            in SvgElement { m | children = ns }
        SvgText t -> SvgText t
        SvgComment c -> SvgComment c

rescale : Int -> SvgNode a -> SvgNode a
rescale p n =
    let st = String.fromInt p
    in case n of
           SvgElement e -> if e.name == "svg"
                           then SvgElement { e | attributes = List.map (setAttr "width" (st ++ "vw"))
                                                              <| List.map (setAttr "height" (st ++ "vh"))
                                                              <| e.attributes
                                           }
                           else n
           _ -> n

testSvg = """
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
 "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<!-- Generated by graphviz version 2.40.1 (20161225.0304)
 -->
<!-- Title: %3 Pages: 1 -->
<svg width="134" height="188"
 viewBox="0.00 0.00 134.00 188.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 184)">
<title>%3</title>
<polygon fill="#ffffff" stroke="transparent" points="-4,4 -4,-184 130,-184 130,4 -4,4"/>
<!-- a -->
<g id="a" class="node">
<title>a</title>
<ellipse fill="none" stroke="#000000" cx="27" cy="-162" rx="27" ry="18"/>
<text text-anchor="middle" x="27" y="-158.3" font-family="Times,serif" font-size="14.00" fill="#000000">a</text>
</g>
<!-- b -->
<g id="b" class="node">
<title>b</title>
<ellipse fill="none" stroke="#000000" cx="63" cy="-90" rx="27" ry="18"/>
<text text-anchor="middle" x="63" y="-86.3" font-family="Times,serif" font-size="14.00" fill="#000000">b</text>
</g>
<!-- a&#45;&gt;b -->
<g id="edge1" class="edge">
<title>a&#45;&gt;b</title>
<path fill="none" stroke="#000000" d="M35.7146,-144.5708C39.9597,-136.0807 45.1536,-125.6929 49.8663,-116.2674"/>
<polygon fill="#000000" stroke="#000000" points="53.024,-117.7782 54.3657,-107.2687 46.763,-114.6477 53.024,-117.7782"/>
</g>
<!-- c -->
<g id="c" class="node">
<title>c</title>
<ellipse fill="none" stroke="#000000" cx="63" cy="-18" rx="27" ry="18"/>
<text text-anchor="middle" x="63" y="-14.3" font-family="Times,serif" font-size="14.00" fill="#000000">c</text>
</g>
<!-- b&#45;&gt;c -->
<g id="edge2" class="edge">
<title>b&#45;&gt;c</title>
<path fill="none" stroke="#000000" d="M63,-71.8314C63,-64.131 63,-54.9743 63,-46.4166"/>
<polygon fill="#000000" stroke="#000000" points="66.5001,-46.4132 63,-36.4133 59.5001,-46.4133 66.5001,-46.4132"/>
</g>
<!-- d -->
<g id="d" class="node">
<title>d</title>
<ellipse fill="none" stroke="#000000" cx="99" cy="-162" rx="27" ry="18"/>
<text text-anchor="middle" x="99" y="-158.3" font-family="Times,serif" font-size="14.00" fill="#000000">d</text>
</g>
<!-- d&#45;&gt;b -->
<g id="edge3" class="edge">
<title>d&#45;&gt;b</title>
<path fill="none" stroke="#000000" d="M90.2854,-144.5708C86.0403,-136.0807 80.8464,-125.6929 76.1337,-116.2674"/>
<polygon fill="#000000" stroke="#000000" points="79.237,-114.6477 71.6343,-107.2687 72.976,-117.7782 79.237,-114.6477"/>
</g>
</g>
</svg>
"""
