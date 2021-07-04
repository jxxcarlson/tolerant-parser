module Parser.TextCursor exposing
    ( TextCursor, init, add, push, pop
    , ErrorStatus(..), ParseError, empty, parseResult
    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import Parser.AST as AST exposing (Element(..))
import Html exposing (a)
import Parser.Loc exposing (end)
import Parser.MetaData exposing (MetaData)


{-| SourceText structure used by Parser.Loop.run as it progressively "eats" bites of
the text, accumulating the parsed bites in the list `parsed: List Expression`.
The `offset` represents the position of beginning of the current `text` in the
original text. It is used to properly construct Expressions, which contain
as a component a SourceMap, which locates the bite of text in the original input
text.
-}
type alias TextCursor =
    { count : Int
    , generation : Int
    , offset : Int
    , length : Int
    --
    , source : String
    , text : String
    , parsed : List Element
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


parseResult : TextCursor -> List Element
parseResult t =
    t.parsed


empty : TextCursor
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
init : Int -> String -> TextCursor
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

type alias StackItem = {expect : Expectation, preceding : List Element, count : Int}

type alias Expectation = { begin : Char, end : Char }

add : String -> TextCursor -> TextCursor
add str tc = 
  let
      _ =  Debug.log "!"  "ADD"
  in
  {tc | count = tc.count + 1
        , text = str  ++ tc.text
        , offset = tc.offset + String.length str
        } 

push : (String -> Element) -> Expectation -> TextCursor -> TextCursor
push parse expectation tc =
  let
      _ =  Debug.log "!"  "PUSH"
  in
  if tc.text == "" then 
    {tc | count = tc.count + 1
        , offset = tc.offset + 1
        , stack = {expect = expectation, preceding = tc.parsed, count = tc.count}::tc.stack
        , parsed = []
        , text = ""
        }
  else
    let
      el = parse tc.text
      
    in
    {tc | count = tc.count + 1
        , offset = tc.offset + 1
        , stack = {expect = expectation, preceding = (el)::tc.parsed, count = tc.count}::tc.stack
        , parsed = []
        , text = ""
        }

pop : (String -> Element) -> TextCursor -> TextCursor
pop parse tc = 
  let
      _ = Debug.log  "!" "POP"
  in
  case List.head tc.stack of 
    Nothing -> tc
    Just item ->
        if tc.text /= "" then
            let
                newParsed = (String.fromChar item.expect.begin )
                            ++ tc.text 
                            ++ (String.fromChar item.expect.end)
                            |> parse
            in 
                {tc | offset = tc.offset + 1
                , count = tc.count + 1
                , parsed = newParsed :: item.preceding ++ tc.parsed
                , stack = List.drop 1 tc.stack
                , text = ""
                }
        else
          case tc.parsed of 
            (first::next::rest) ->
              case next of 
                AST.Raw txt _ ->
                  {tc | parsed = Element (AST.Name txt)[] first Parser.MetaData.dummy::item.preceding ++  rest
                        , offset = tc.offset + 1
                        , count = tc.count + 1
                        , stack = List.drop 1 tc.stack
                        , text = "" }
                _ -> {tc | offset = tc.offset + 1, count = tc.count + 1}
            _ -> {tc | offset = tc.offset + 1, count = tc.count + 1}
               
