module Svg.Parser exposing
    ( SvgNode(..), Element, SvgAttribute(..)
    , parse, toSvg, toAttribute
    )

import Char
import Parser exposing (..)
import Set
import Result
import Debug

import Html exposing (Html)
import Svg exposing (Attribute, Svg)
import VirtualDom


{-| A SVG node can be one of the three: SvgElement, SvgText or SvgComment.
-}
type SvgNode msg
    = SvgElement (Element msg)
    | SvgText String
    | SvgComment String


{-| An Element consists of a tag name, a list of attributes, a list of children nodes.

    <svg xmlns="http://www.w3.org/2000/svg"></svg>

will be parsed as

    Element "svg" [ ( "xmlns", "http://www.w3.org/2000/svg" ) ] []

-}
type alias Element msg =
    { name : String
    , attributes : List (SvgAttribute msg)
    , children : List (SvgNode msg)
    }


{-| Can represent a name/value pair to denote and attribute,
    and can also represent an event attribute
-}
type SvgAttribute msg
    = SimpleAttr String String
    | EventAttr (Attribute msg)

{-| Parses `String` to `SvgNode msg`. This will parse a single svg node.

    parse "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>"
        == Ok (SvgElement (Element "svg" [ ( "xmlns", "http://www.w3.org/2000/svg" ) ] []))

-}
parse : String -> Result String (SvgNode msg)
parse str = run svg str
          |> Result.mapError (Debug.toString)

{-| Convert `SvgNode msg` to `Svg msg`. This is useful when you want to manipulate `SvgNode msg` before conveting to `Html msg`.
-}
toSvg : SvgNode msg -> Svg msg
toSvg svgNode =
    case svgNode of
        SvgElement element ->
            elementToSvg element

        SvgText content ->
            Svg.text content

        SvgComment content ->
            Svg.text ""

{-| Convert `SvgAttribute msg` to `Attribute msg`. This is useful when you want to manipulate `SvgAttribute msg` before converting to `Html msg`.
-}
toAttribute : SvgAttribute msg -> Attribute msg
toAttribute a =
    case a of
        SimpleAttr k v -> VirtualDom.attribute k v
        EventAttr attr -> attr

attrKey : Parser String
attrKey =
    variable
    { start = Char.isAlpha
    , inner = \c -> Char.isAlphaNum c || c == '_' || c == '-' || c == ':'
    , reserved = Set.empty
    }

tagName = attrKey

isWhiteSpace c = c == ' ' || c == '\n' || c == '\r'

space : Parser ()
space =
    succeed ()
        |. chompIf isWhiteSpace
        |. chompWhile isWhiteSpace

string : Parser String
string =
    succeed identity
        |. symbol "\""
        |= (getChompedString <| chompUntil "\"")
        |. symbol "\""

attribute : Parser (SvgAttribute msg)
attribute =
    succeed SimpleAttr
        |= attrKey
        |. spaces
        |. symbol "="
        |. spaces
        |= string

attributes : Parser (List (SvgAttribute msg))
attributes = loop [] attribHelper

attribHelper : List (SvgAttribute msg) -> Parser (Step (List (SvgAttribute msg)) (List (SvgAttribute msg)))
attribHelper revAttrs =
    oneOf
    [ succeed (\attr -> Loop (attr :: revAttrs))
        |. backtrackable space
        |= attribute
    , succeed ()
        |> map (\_ -> Done (List.reverse revAttrs))
    ]

node : Parser (SvgNode msg)
node =
    succeed identity
        |. backtrackable spaces
        |. symbol "<"
        |= oneOf
           [ succeed SvgComment
               |. symbol "!--"
               |= (getChompedString <| chompUntil "-->")
               |. symbol "-->"
           , tagName
               |> andThen (\tag -> succeed (\a c -> SvgElement (Element tag a c))
                                      |= attributes
                                      |. spaces
                                      |= closingOrChild tag
                                   )
           ]

closingOrChild : String -> Parser (List (SvgNode msg))
closingOrChild tag =
    oneOf
    [ succeed []
        |. symbol "/>"
    , succeed identity
        |. symbol ">"
        |. spaces
        |= loop [] (childHelper tag)
    ]

childHelper : String -> List (SvgNode msg) -> Parser (Step (List (SvgNode msg)) (List (SvgNode msg)))
childHelper tag revChilds =
    oneOf
    [ succeed (Done (List.reverse revChilds))
        |. backtrackable spaces
        |. symbol "</"
        |. spaces
        |. symbol tag
        |. spaces
        |. symbol ">"
    , succeed (\c -> Loop (c :: revChilds))
        |= oneOf
           [ node
           , succeed SvgText
               |= (getChompedString <| chompUntil "<")
           ]
    ]

xmlheader : Parser ()
xmlheader =
    succeed ()
        |. spaces
        |. symbol "<?xml"
        |. chompUntil "?>"
        |. symbol "?>"

doctype : Parser ()
doctype =
    succeed ()
        |. spaces
        |. symbol "<!DOCTYPE"
        |. chompUntil ">"
        |. symbol ">"

headerComments : Parser ()
headerComments = loop () commentHelper

commentHelper : () -> Parser (Step () ())
commentHelper _ =
    oneOf
    [ succeed (Loop ())
        |. backtrackable spaces
        |. symbol "<!--"
        |. chompUntil "-->"
        |. symbol "-->"
    , succeed ()
        |> map (\_ -> Done ())
    ]

header : Parser ()
header =
    oneOf
    [succeed ()
        |. xmlheader
        |. doctype
        |. headerComments
    , succeed ()
    ]

testparse : String -> String
testparse str =
    case run xmlheader str of
        Ok v -> Debug.toString v
        Err err -> Debug.toString err

elementToSvg : Element msg -> Svg msg
elementToSvg element =
    Svg.node element.name
        (List.map toAttribute element.attributes)
        (List.map toSvg element.children)

svg : Parser (SvgNode msg)
svg =
    succeed identity
        |. header
        |= node
