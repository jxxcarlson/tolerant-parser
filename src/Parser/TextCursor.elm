module Parser.TextCursor exposing
    ( TextCursor, init, add, push, pop
    , ErrorStatus(..), ParseError, empty, parseResult
    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import Parser.AST as AST exposing (Element)
import Html exposing (a)


{-| SourceText structure used by Parser.Loop.run as it progressively "eats" bites of
the text, accumulating the parsed bites in the list `parsed: List Expression`.
The `offset` represents the position of beginning of the current `text` in the
original text. It is used to properly construct Expressions, which contain
as a component a SourceMap, which locates the bite of text in the original input
text.
-}
type alias TextCursor a =
    { count : Int
    , generation : Int
    , offset : Int
    , length : Int
    --
    , source : String
    , text : String
    , parsed : List a
    , stack : List StackItem
    }



-- summary : TextCursor Element -> { text : b, block : c, parsed : List (List Element.SimpleElement), stack : d }


type alias ParseError =
    { status : ErrorStatus, correctedText : List String }


type ErrorStatus
    = NoError
    | PipeError
    | RightBracketError
    | LeftBracketError
    | UnhandledError


parseResult : TextCursor a -> List a
parseResult t =
    t.parsed


empty : TextCursor a
empty =
    { count = 0
    , generation = 0
    , offset = 0
    , length = 0
    --
    , source = ""
    , text = ""
    , parsed = []
    , stack = []
    }


{-| Return a TextCursor with given chunkNumber and text
-}
init : Int -> String -> TextCursor a
init generation source =
    { count = 0
    , generation = generation
    , offset = 0
    , length = String.length source
    --
    , source = source
    , text = ""
    , parsed = []
    , stack = []
    }

type alias StackItem = {expect : Expectation, preceding : List String}

type alias Expectation = { start : Char, finish : Char }

add : String -> TextCursor a -> TextCursor a
add str tc = 
  ({tc | count = tc.count + 1
        , text = str 
        , offset = tc.offset + String.length str
        }) |> Debug.log "ADD" 

push : (String -> a) -> Expectation -> TextCursor a -> TextCursor a
push parse expectation tc =
  ({tc | count = tc.count + 1
       , offset = tc.offset + 1
       , stack = {expect = expectation, preceding = [tc.text]}::tc.stack
       , parsed = parse tc.text :: tc.parsed
       , text = ""
       }) |> Debug.log "PUSH" 

pop : (String -> a) -> TextCursor a -> TextCursor a
pop parse tc = 
  (case List.head tc.stack of 
    Nothing -> tc
    Just item ->
      let
          
          newParsed = (String.fromChar item.expect.start )
                     ++ tc.text 
                     ++ (String.fromChar item.expect.finish)
                     |> parse
      in 
        {tc | offset = tc.offset + 1
        , parsed = newParsed :: tc.parsed
        , stack = List.drop 1 tc.stack
        , text = ""}) |> Debug.log "POP"
