module Parser.Driver exposing (..)

import Parser.AST as AST exposing (Element(..))
import Parser.Advanced as PA
import Parser.Error exposing (Context(..), Problem(..))
import Parser.Loop as Loop
import Parser.Parser as Parser
import Parser.TextCursor as TextCursor exposing (ErrorStatus(..), TextCursor)


{-| The value of Loop.Packet that we need here
-}
packet : Loop.Packet Element
packet =
    { parser = Parser.parser
    , getLength = AST.length
    , handleError = Nothing
    }


parseLoop : Int -> String -> TextCursor Element
parseLoop generation str =
    Loop.parseLoop packet generation str


pl : String -> List AST.Element_
pl str =
    let
        tc =
            parseLoop 0 str

        _ =
            Debug.log "FINAL OFFSET" tc.offset
    in
    tc |> .parsed |> List.map AST.simplify



-- ERROR HANDLER


type alias ParseError =
    PA.DeadEnd Context Problem



--
--
--{-| Transform the text cursor so that it does not have errors.
---}
--handleError : List ParseError -> TextCursor Element -> TextCursor Element
--handleError errors tc =
--    let
--        mFirstError =
--            List.head errors
--
--        problem : Problem
--        problem =
--            mFirstError |> Maybe.map .problem |> Maybe.withDefault (UnHandledError 0)
--
--        textLines =
--            String.lines tc.text
--
--        smLength =
--            Maybe.map String.length (List.head textLines) |> Maybe.withDefault 0
--
--        meta =
--            { blockOffset = 0
--            , offset = 0
--            , length = smLength
--            , generation = tc.generation
--            , label = "problem"
--            }
--
--        errorColumn =
--            mFirstError |> Maybe.map .col |> Maybe.withDefault 0
--
--        errorRow =
--            Maybe.map .row mFirstError |> Maybe.withDefault 0
--    in
--    { text = text
--    , parsed = parsand :: tc.parsed -- newElement :: List.drop 1 tc.parsed -- throw away the erroneous parsand
--    , stack = []
--    , offset = tc.offset + smLength -- TODO: trouble!?
--    , count = tc.count
--    , generation = tc.generation
--    , error = { status = NoError, correctedText = [] }
--    }
