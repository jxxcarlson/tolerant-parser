module Parser.Parser exposing (..)

{-


-}

import Maybe.Extra
import Parser.AST as AST exposing (Element(..), Name(..))
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Loc2 as Loc exposing (Position)
import Parser.MetaData as MetaData exposing (MetaData)
import Parser.RawString as RawString
import Parser.Tool as T
import Parser.XString as XString


type alias ErrorMessage =
    String


type alias Parser a =
    Parser.Parser Context Problem a


type alias ParseError =
    Parser.DeadEnd Context Problem


type CYTMsg
    = Mark2Msg
    | CYDocumentLink String



-- PARSER


p =
    parse 0


p2 =
    p >> AST.simplify


parse : Int -> String -> Element
parse generation str =
    case Parser.run (parser generation) str of
        Ok ast ->
            ast

        Err errors ->
            Problem errors str


parseList : Int -> Int -> String -> Result (List ParseError) (List Element)
parseList generation lineNumber str =
    Parser.run (listParser generation lineNumber) str


listParser : Int -> Int -> Parser (List Element)
listParser generation lineNumber =
    T.many (parser generation)


parser : Int -> Parser Element
parser generation =
    Parser.oneOf [ primitiveElement generation, text generation ]


{-|

> run (primitiveElement 0 0) "[strong |0| stuff]"
> Ok (Element "strong" ["0"] (" stuff") (Just { blockOffset = 0, content = "[strong |0| stuff]", generation = 0, length = 18, offset = 0 }))

> run (primitiveElement 0 0) "[strong stuff]"
> Ok (Element "strong" [] "stuff" (Just { blockOffset = 0, content = "[strong stuff]", generation = 0, length = 14, offset = 0 }))

-}
primitiveElement : Int -> Parser Element
primitiveElement generation =
    Parser.inContext CElement <|
        -- Parser.succeed (\start name ( args, body_ ) end source -> Element name args body_ (Just { generation = generation, blockOffset = blockOffset, offset = start, length = end - start }))
        -- TODO: is this correct?
        Parser.succeed (\start name ( args, body_ ) end source -> Element name args body_ (meta generation start end))
            |= Parser.getOffset
            |. leftBracket
            |= Parser.oneOf [ elementName |> Parser.map Name, Parser.succeed Undefined ]
            |= argsAndBody generation
            |. Parser.spaces
            |. rightBracket
            |= Parser.getOffset
            |= Parser.getSource


elementName =
    T.first (string_ [ '[', ']', ' ', '\n' ]) Parser.spaces


argsAndBody generation =
    Parser.inContext CArgsAndBody <|
        Parser.oneOf [ argsAndBody_ generation, bodyOnly generation ]


elementArgs =
    Parser.inContext CArgs <|
        T.between pipeSymbol innerElementArgs pipeSymbol


innerElementArgs =
    T.manySeparatedBy comma (string [ ',', '|' ])



-- elementBody : Int -> Parser.Parser Context Problem Element
-- elementBody : Int -> Parser.Parser Context Problem (MetaData -> Element)


metaOfList generation list =
    { generation = generation, position = list |> List.map (\el -> AST.position el) |> Loc.positionOfList }


elementBody generation =
    Parser.inContext CBody <|
        Parser.lazy (\_ -> T.many (parser generation) |> Parser.map (\list -> EList list (metaOfList generation list)))


argsAndBody_ generation =
    Parser.succeed (\args body_ -> ( args, body_ ))
        |= elementArgs
        |. Parser.spaces
        |= elementBody generation


bodyOnly generation =
    Parser.succeed (\body_ -> ( [], body_ ))
        |= elementBody generation



-- TOOLS


isProblem : Element -> Bool
isProblem element =
    case element of
        Problem _ _ ->
            True

        _ ->
            False


hasProblem : List Element -> Bool
hasProblem elements =
    List.foldl (\e acc -> isProblem e || acc) False elements



-- TEXT AND STRINGS


text : Int -> Parser Element
text generation =
    Parser.oneOf [ rawString generation, plainText generation ]


meta generation start finish =
    { position = { start = start, end = finish }, generation = generation }


rawString : Int -> Parser Element
rawString generation =
    Parser.succeed (\start source finish -> Raw source (meta generation start finish))
        |= Parser.getOffset
        |= RawString.parser
        |= Parser.getOffset


plainText : Int -> Parser Element
plainText generation =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate XString.isNonLanguageChar
            |> Parser.map (\data -> Raw data.content (meta generation data.start data.finish))
        )


textWithPredicate : (Char -> Bool) -> Int -> Parser Element
textWithPredicate predicate generation =
    Parser.inContext TextExpression <|
        (XString.textWithPredicate predicate
            |> Parser.map (\data -> Raw data.content (meta generation data.start data.finish))
        )


type alias StringData =
    { content : String, start : Int, finish : Int }


string stopChars =
    T.first (string_ stopChars) Parser.spaces


string_ : List Char -> Parser String
string_ stopChars =
    rawText_ stopChars |> Parser.map .content


rawText_ : List Char -> Parser { start : Int, length : Int, content : String }
rawText_ stopChars =
    Parser.succeed (\begin end content -> { start = begin, length = end - begin, content = String.slice begin end content })
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> not (List.member c stopChars))
        |= Parser.getOffset
        |= Parser.getSource


comma_ =
    Parser.symbol (Parser.Token "," ExpectingComma)


comma =
    T.first comma_ Parser.spaces


rawStringBegin =
    Parser.symbol (Parser.Token "r##" ExpectingRawStringBegin)


rawStringEnd =
    Parser.symbol (Parser.Token "##" ExpectingRawStringEnd)


pipeSymbol =
    Parser.symbol (Parser.Token "|" ExpectingPipe)


leftBracket =
    Parser.symbol (Parser.Token "[" ExpectingLeftBracket)


rightBracket =
    Parser.symbol (Parser.Token "]" ExpectingRightBracket)



--{-|
--
--    Use this to parse a string and return information about its location in the source
--
---}
--
-- getChompedString : Int -> Int -> Parser a -> ( Parser String, MetaData )
--
--
--getChompedString generation offset parser_ =
--    let
--        sm first_ last_ source_ =
--            let
--                src =
--                    String.slice first_ last_ source_
--            in
--            -- ( src, Just { blockOffset = lineNumber, length = last_, offset = 0, generation = generation } )
--            -- TODO: is the below correct?
--            ( src, MetaData.init generation offset)
--    in
--    Parser.succeed sm
--        |= Parser.getOffset
--        |. parser_
--        |= Parser.getOffset
--        |= Parser.getSource

