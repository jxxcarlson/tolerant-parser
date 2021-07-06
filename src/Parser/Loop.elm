module Parser.Loop exposing (Packet, parseLoop, advance)

import Parser.AST as AST exposing (Element)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context, Problem)
import Parser.TextCursor as TextCursor exposing (TextCursor)
import Parser.Tool
import Parser.AST exposing (simplify)


type alias Parser a =
    Parser.Parser Context Problem a


type alias Packet a =
    { parser : String ->  a
    , getLength : a -> Int
    , handleError : Maybe (List (Parser.DeadEnd Context Problem) -> TextCursor -> TextCursor)
    }


{-| parseLoop takes as input an integer representing a "chunkOffset" and
a string of source text, the "chunk." It returns a TextCursor, which
is a data structure which includes the parsed source text.
-}
parseLoop : Packet Element -> Int -> String -> TextCursor
parseLoop packet generation str =
    Parser.Tool.loop (TextCursor.init generation str) (nextCursor packet)
      |> TextCursor.commit


{-| nextCursor operates by running the expression parser on
`tc.text` with argument `tc.chunkNumber`. This argument is used
to track the location in the source text of the piece of text
parsed.

Recall that parseLoop is fed chunks of text by
Document.process. These chunks are "logical paragraphs,"
which one may think of as forming an array of strings indexed
by chunkNumber. A piece of text within a chunk is identified
by an offset and a length:

    piece =
        String.slice offset (offset + length) chunk

If parsing succeeds, resulting in a parsand `expr`, the textCursor
operated by parseLoop is updated:

    - the "program counter" tc.count is incremented
    - the piece of text corresponding to the parsand
      is removed from tc.text
    - `expr` is prepended to `tc.parsed`

-}
nextCursor : Packet Element -> TextCursor -> Parser.Tool.Step (TextCursor) (TextCursor)
nextCursor packet tc =
    let
        p = tc.parsed |> List.map AST.simplify
        co  = tc.complete |> List.map AST.simplify
        _ = Debug.log "(count, offset, remaining)" (tc.count, tc.offset, String.dropLeft tc.offset tc.source)
        _ = Debug.log ("TC "  ++ String.fromInt tc.count) {p = p, c = co, s = tc.stack |> List.map TextCursor.simpleStackItem, t = tc.text}
        _ = Debug.log "-" "-------------------------------------------------"
    in
    if tc.offset >= tc.length  then
        Parser.Tool.Done tc


    else
      let
         remaining = String.dropLeft tc.offset tc.source
         chompedText = advance remaining 
         n = chompedText.finish - chompedText.start
         firstChar = String.uncons remaining |> Maybe.map Tuple.first
         
       in
       if n > 0 then
         Parser.Tool.Loop <| TextCursor.add chompedText.content tc
       else
         case firstChar of
            Nothing -> Parser.Tool.Done tc
            Just c -> 
              if c == '[' then
                Parser.Tool.Loop <| TextCursor.push packet.parser {begin = '[', end = ']'} tc  
              else if c == ']' then 
                Parser.Tool.Loop <| TextCursor.pop packet.parser tc  
              else                                                                                                 
                -- Parser.Tool.Loop <| TextCursor.pop tc 
                Parser.Tool.Done tc
   

-- advance = Parser.Tool.text (\c -> c /= '.') (\c -> c /= '.' ) 


notDelimiter c = not (List.member c ['[', ']'])
advance str = 
  case Parser.run (Parser.Tool.text (\c -> notDelimiter c) (\c -> notDelimiter c )) str of
     Ok stringData -> stringData
     Err _ ->  {content = "", finish = 0, start = 0}
