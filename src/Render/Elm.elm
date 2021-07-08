module Render.Elm exposing (render, renderList)

import Dict exposing (Dict)
import Element as E exposing (column, el, fill, paragraph, px, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Parser.AST exposing (Element(..), Element_(..), Name(..))
import Parser.Advanced
import Parser.Driver
import Parser.Error exposing (Context(..), Problem(..))


type alias ParseError =
    Parser.Advanced.DeadEnd Context Problem


type alias RenderArgs =
    { width : Int
    , selectedId : String
    , generation : Int
    }


type alias FRender msg =
    RenderArgs -> String -> List String -> Element -> E.Element msg


type alias RenderElementDict msg =
    Dict String (FRender msg)


renderElementDict : RenderElementDict msg
renderElementDict =
    Dict.fromList
        [ ( "i", italic )
        , ( "b", bold )
        ]


renderList : RenderArgs -> List Element -> List (E.Element msg)
renderList renderArgs list =
    List.map (render renderArgs) list


render : RenderArgs -> Element -> E.Element msg
render renderArgs element =
    case element of
        Raw str _ ->
            E.el [] (text str)

        Element (Name name) _ body _ ->
            renderWithDictionary renderArgs name [] body

        Element Undefined _ body _ ->
            E.el [] (text <| "Undefined element")

        EList elements _ ->
            E.paragraph [ E.width (px 600) ] (List.map (render renderArgs) elements)

        Problem _ str ->
            el [] (text <| "PROBLEM: " ++ str)

        StackError _ _ message errorText ->
            paragraph [] [ el [ Background.color (rgb255 255 255 0) ] (text errorText), el [ Font.bold, Font.color (rgb255 0 0 200) ] (text <| " " ++ message) ]

        Empty ->
            el [] (text <| "EMPTY")


renderWithDictionary renderArgs name args body =
    case Dict.get name renderElementDict of
        Just f ->
            f renderArgs name args body

        Nothing ->
            E.paragraph [ spacing 8 ]
                [ el [ Font.color (rgb255 200 0 0), Font.bold ] (text name)
                , el [] (text " ")
                , render renderArgs body
                ]


italic : FRender msg
italic renderArgs _ _ body =
    el [ Font.italic ] (render renderArgs body)


bold : FRender msg
bold renderArgs _ _ body =
    el [ Font.bold ] (render renderArgs body)
