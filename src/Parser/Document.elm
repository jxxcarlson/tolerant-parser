module Parser.Document exposing (..)

import Parser.AST exposing (Element)
import Parser.Driver


parse : Int -> String -> List (List Element)
parse generation str =
    str
        |> split
        |> List.map (Parser.Driver.parse generation)


split : String -> List String
split str =
    String.split "\n\n" str
        |> List.filter (\s -> s /= "")
