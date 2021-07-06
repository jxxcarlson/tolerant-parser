module Parser.AST exposing (Element(..), Element_(..), simplify, Name(..), position, length)

import Parser.Advanced as Parser
import Parser.Error exposing (..)
import Parser.Loc2 as Loc
import Parser.MetaData exposing (MetaData)


type Element
    = Raw String MetaData
    | Element Name (List String) Element MetaData
    | EList (List Element) MetaData
    | Problem (List ParseError) String
    | Empty


length : Element -> Int
length element =
    let
        pos =
            position element
    in
    pos.end - pos.start


position : Element -> Loc.Position
position element =
    case element of
        Raw _ meta ->
            meta.position

        Element _ _ _ meta ->
            meta.position

        EList _ meta ->
            meta.position

        Problem _ _ ->
            Loc.dummy

        Empty ->
            Loc.dummy


type alias ParseError =
    Parser.DeadEnd Context Problem


type Name
    = Name String
    | Undefined


type Element_
    = Raw_ String
    | Element_ Name (List String) Element_
    | EList_ (List Element_)
    | Problem_ Problem String
    | Incomplete_


simplify : Element -> Element_
simplify element =
    case element of
        Raw str _ ->
            Raw_ str

        Element name strList el _ ->
            Element_ name strList (simplify el)

        EList elementList _ ->
            EList_ (List.map simplify elementList)

        Problem p s ->
            Problem_ (List.head p |> Maybe.map .problem |> Maybe.withDefault NoError) s

        Empty ->
            Incomplete_
