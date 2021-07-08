module Render.Elm exposing (render, renderList)

import Dict exposing (Dict)
import Element as E exposing (column, el, fill, paragraph, px, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode
import Parser.AST exposing (Element(..), Element_(..), Name(..))
import Parser.Advanced
import Parser.Driver
import Parser.Error exposing (Context(..), Problem(..))
import Utility


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
        , ( "strike", strike )
        , ( "underline", underline )
        , ( "hide", hide )
        , ( "highlight", highlight )
        , ( "highlightRGB", highlight )

        --, ( "fontRGB", fontRGB )
        --, ( "red", red )
        --, ( "blue", blue )
        --, ( "violet", violet )
        --, ( "medgray", medgray )
        --, ( "code", renderCode )
        --, ( "c", renderCode )
        , ( "math", renderMath )
        , ( "m", renderMath )
        , ( "mathblock", renderMathDisplay )
        , ( "mb", renderMathDisplay )
        , ( "link", link )
        , ( "image", image )
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



-- TEXT STYLE


italic : FRender msg
italic renderArgs _ _ body =
    el [ Font.italic ] (render renderArgs body)


bold : FRender msg
bold renderArgs _ _ body =
    el [ Font.bold ] (render renderArgs body)


strike : FRender msg
strike renderArgs _ _ body =
    el [ Font.strike ] (render renderArgs body)


underline : FRender msg
underline renderArgs _ _ body =
    el [ Font.underline ] (render renderArgs body)


hide : FRender msg
hide renderArgs _ _ body =
    E.none


highlight : FRender msg
highlight renderArgs _ _ body =
    el [ Background.color yellowColor, E.paddingXY 4 2 ] (render renderArgs body)



--fontRGB : FRender msg
--fontRGB renderArgs _ _ body =
--    let
--        args =
--            getText body |> Maybe.withDefault "nada" |> String.words
--
--        toInt x =
--            String.toInt x |> Maybe.withDefault 0
--
--        {r } =
--            case args of
--                r_ :: g_ :: b_ :: rest ->
--                    ( toInt r_, toInt g_, toInt b_ )
--
--                _ ->
--                    ( 0, 0, 0 )
--    in
--    el [ Font.color (E.rgb255 r g b), E.paddingXY 4 2 ] (render renderArgs body)


link : FRender msg
link renderArgs name args body =
    let
        bodyText =
            getText body |> Maybe.withDefault "missing url"

        ( label, url ) =
            case String.words bodyText of
                label_ :: url_ :: rest ->
                    ( label_, url_ )

                url_ :: [] ->
                    ( url_, url_ )

                [] ->
                    ( "no label", "https://nowhere.com" )
    in
    E.newTabLink []
        { url = url
        , label = el [ Font.color linkColor, Font.italic ] (text label)
        }


image : FRender msg
image renderArgs name _ body =
    let
        args_ =
            getText body |> Maybe.withDefault "" |> String.words

        args =
            List.take (List.length args_ - 1) args_

        url =
            List.head (List.reverse args_) |> Maybe.withDefault "no-image"

        dict =
            Utility.keyValueDict args

        description =
            Dict.get "caption" dict |> Maybe.withDefault ""

        caption =
            case Dict.get "caption" dict of
                Nothing ->
                    E.none

                Just c ->
                    E.row [ placement, E.width E.fill ] [ el [ E.width E.fill ] (text c) ]

        width =
            case Dict.get "width" dict of
                Nothing ->
                    px displayWidth

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            px displayWidth

                        Just w ->
                            E.px w

        placement =
            case Dict.get "placement" dict of
                Nothing ->
                    E.centerX

                Just "left" ->
                    E.alignLeft

                Just "right" ->
                    E.alignRight

                Just "center" ->
                    E.centerX

                _ ->
                    E.centerX

        displayWidth =
            renderArgs.width
    in
    E.column [ spacing 8, E.width (E.px displayWidth), placement ]
        [ E.image [ E.width width, placement ]
            { src = url, description = description }
        , caption
        ]


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


renderMathDisplay : FRender msg
renderMathDisplay rendArgs name args body =
    case getText body of
        Just content ->
            mathText rendArgs DisplayMathMode content

        Nothing ->
            el [ Font.color redColor ] (text "Error rendering math !!!")


renderMath : FRender msg
renderMath renderArgs name args body =
    case getText body of
        Just content ->
            mathText renderArgs InlineMathMode content

        Nothing ->
            el [ Font.color redColor ] (text "Error rendering math !!!")


mathText : RenderArgs -> DisplayMode -> String -> E.Element msg
mathText renderArgs displayMode content =
    Html.Keyed.node "span"
        []
        [ ( String.fromInt renderArgs.generation, mathText_ displayMode renderArgs.selectedId content )
        ]
        |> E.html


mathText_ : DisplayMode -> String -> String -> Html msg
mathText_ displayMode selectedId content =
    Html.node "math-text"
        -- active meta selectedId  ++
        [ HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string content)

        -- , clicker meta
        -- , HA.id (makeId meta)
        ]
        []


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True


getText : Element -> Maybe String
getText element =
    case element of
        EList [ Raw content _ ] _ ->
            Just content

        _ ->
            Nothing



-- COLORS


linkColor =
    E.rgb 0 0 0.8


blackColor =
    E.rgb 0 0 0


medGray =
    E.rgb 0.4 0.4 0.4


redColor =
    E.rgb 0.7 0 0


blueColor =
    E.rgb 0 0 0.8


darkBlueColor =
    E.rgb 0 0 0.6


yellowColor =
    E.rgb 1.0 1.0 0


violetColor =
    E.rgb 0.4 0 0.8


codeColor =
    -- E.rgb 0.2 0.5 1.0
    E.rgb 0.4 0 0.8
