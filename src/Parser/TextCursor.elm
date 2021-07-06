module Parser.TextCursor exposing
    ( TextCursor, init, add, push, pop, commit, simpleStackItem
    , ErrorStatus(..), ParseError, empty, parseResult, firstWord
    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import Parser.AST as AST exposing (Element(..))
import Html exposing (a)
import Parser.Loc exposing (end)
import Parser.MetaData exposing (MetaData)
import Parser.AST exposing (simplify)


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
    , complete : List Element
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
    , complete = []
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
    , complete = []
    , stack = []
    }

type alias StackItem = {expect : Expectation, data : String, count : Int}


simpleStackItem : StackItem -> String
simpleStackItem { data, count } = String.fromInt count ++ ": " ++ data

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
    case tc.stack of 
        [] -> 
            let
                complete = if tc.text /= "" then 
                    parse tc.text::tc.parsed ++ tc.complete
                    else 
                    tc.parsed ++ tc.complete
                
                _ = tc.text |> Debug.log "TXT"
            --  _ = parse tc.text |> Debug.log "PARSED TXT"
            in
            
            {tc | count = tc.count + 1
                , offset = tc.offset + 1
                , stack = {expect = expectation, data = tc.text, count = tc.count}::tc.stack
                , parsed = []
                , complete = complete |> Debug.log "PUSH, COMPLETE"
                , text = ""
                }
        (first::rest) ->
            let 
                first_ = if String.trim tc.text == "" then 
                           first 
                         else
                           {first | data = tc.text }
                --- ^^ TODO: HACKY!
                in
                {tc | count = tc.count + 1
                    , offset = tc.offset + 1
                    , stack = {expect = expectation, data = "", count = tc.count}::first_::rest
                    , text = ""
                    }

pop : (String -> Element) -> TextCursor -> TextCursor
pop parse tc = 
  let
      _ = Debug.log  "!" "POP"
  in
  case List.head tc.stack of 
    Nothing -> {tc | count = tc.count + 1, offset = tc.offset + 1}
    Just item ->
        if tc.text /= "" then
            let
                newParsed = 
                  (String.fromChar item.expect.begin )
                            ++ tc.text 
                            ++ (String.fromChar item.expect.end)
                            |> parse
                parsed = newParsed :: tc.parsed
                stack = List.drop 1 tc.stack

            in 
              if stack == [] then 
                {tc | offset = tc.offset + 1
                , count = tc.count + 1
                , parsed = []
                , stack =  []
                , complete = parsed ++ tc.complete
                , text = ""
                }
              else
                {tc | offset = tc.offset + 1
                , count = tc.count + 1
                , parsed = parsed
                , stack = stack
                , text = ""
                }

        else -- tc.text is empty
            case List.head tc.stack of 
              Nothing -> {tc | count = tc.count + 1, offset = tc.offset + 1}
              Just stackItem -> 
                let
                    newParsed = Element (AST.Name stackItem.data) 
                        [] 
                        (EList (List.reverse tc.parsed) Parser.MetaData.dummy)
                        Parser.MetaData.dummy
                in
                    {tc | parsed = []
                        , complete = newParsed :: tc.complete
                        , stack = (List.drop 1 tc.stack)
                        , offset = tc.offset + 1
                        , count = tc.count + 1
                        , text = "" }

               


-- commit :  TextCursor -> List Element
-- commit tc =
--     let
--         _ = Debug.log "!" ("COMMIT " ++ String.fromInt tc.count)

--         parsed =
--             if tc.text == "" then
--                  tc.parsed 

--             else
--                 (AST.Raw tc.text Parser.MetaData.dummy):: ( tc.parsed)

--     in
--     case tc.stack of
--         [] ->
--             parsed |> Debug.log "COMMIT 1"

--         top :: restOfStack ->
--             let
--                 -- problem =
--                 --     Problem.AnnotationDoesNotFinish
--                 --         { startMark = startMark
--                 --         , expectedEndMark = expectedEndMark
--                 --         , end = position offset end
--                 --         }
                

--                 _ = Debug.log "AT END, STACK SIZE" (1 + List.length restOfStack)
--                 _ = Debug.log "TOP OF STACK" top
--                 _ = Debug.log "PARSED" (tc.parsed |> List.map AST.simplify)

--                 newParsed =
--                     AST.Incomplete :: parsed  ++ top.data 
--             in
--             commit { tc | count = 1 + tc.count, text = ""
--                 , stack = restOfStack
--                 , parsed = newParsed } 
--                 |> Debug.log "COMMIT 2"



commit :  TextCursor -> TextCursor
commit tc = 
  tc |> commit_ |> (\tc2 ->  {tc2 | complete = List.reverse tc2.complete })

 
commit_:  TextCursor -> TextCursor
commit_ tc =
    let
        parsed =
            if tc.text == "" then
                 tc.parsed 

            else
                (AST.Raw tc.text Parser.MetaData.dummy):: ( tc.parsed)

        complete = parsed ++ tc.complete 
        
        _ = complete |> List.map simplify |> Debug.log "!! COMPLETE"


    in
    case tc.stack of
        [] ->
            {tc | parsed = [], complete = complete  } 

        top :: restOfStack ->
            let
                -- problem =
                --     Problem.AnnotationDoesNotFinish
                --         { startMark = startMark
                --         , expectedEndMark = expectedEndMark
                --         , end = position offset end
                --         }
                

                _ = Debug.log "AT END, STACK SIZE" (1 + List.length restOfStack)
                _ = Debug.log "TOP OF STACK" top
                _ = Debug.log "PARSED" (tc.parsed |> List.map AST.simplify)

                newParsed =
                    AST.Empty :: parsed --- more here
            in
            commit { tc | count = 1 + tc.count, text = ""
                , stack = restOfStack
                , parsed = newParsed
                 } 


-- type alias StackItem = {expect : Expectation, data : List Element, count : Int}

-- type alias Expectation = { begin : Char, end : Char }


firstWord : String -> {first : String, rest: String}
firstWord str = 
  let
    words = String.words str
  in
  case words of 
    [] -> {first = "", rest = ""}
    a::rest -> {first = a, rest = String.join " " rest}