module ParseLoopTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parser.Driver exposing(pl)
import Parser.AST exposing(Element_(..),Name(..))


suite : Test
suite =
    describe "The Driver.parseLoop function"
            [ test "pure text input" <|
                \_ ->
                     "foo"
                        |> pl
                        |> Expect.equal [Raw_ "foo"]

            , test "simple element" <|
                \_ ->
                    "[foo]"
                        |> pl
                        |> Expect.equal [Element_ (Name "foo") [] (EList_ [])]


            ,  test "element with two interior pieces" <|
                \_ ->
                    "[i foo]"
                        |> pl
                        |> Expect.equal [Element_ (Name "i") [] (EList_ [Raw_ "foo"])]
            ,   test "nested elements" <|
                \_ ->
                    "[i [b foo]]"
                        |> pl
                        |> Expect.equal [Element_ (Name ("i ")) [] (Element_ (Name "b") [] (EList_ [Raw_ "foo"]))]
            ,  test "simple element preceded by text" <|
                \_ ->
                    "abc [foo]"
                        |> pl
                        |> Expect.equal [Raw_ ("abc "),Element_ (Name "foo") [] (EList_ [])] 

            , test "simple element preceded and followed by text" <|
                \_ ->
                    "abc [foo] def"
                        |> pl
                        |> Expect.equal [Raw_ ("abc "),Element_ (Name "foo") [] (EList_ []), Raw_ (" def")] 

            ,  test "simple element preceded and followed by text (2)" <|
                \_ ->
                    "abc def [foo] ghi jkl [bar] mno pqr"
                        |> pl
                        |> Expect.equal 
                          [Raw_ ("abc def "),Element_ (Name "foo") [] (EList_ []),Raw_ (" ghi jkl "),Element_ (Name "bar") [] (EList_ []),Raw_ (" mno pqr")]           
                          
    ]


-- [[Raw_ (" ghi jkl"),Element_ (Name "foo") [] (EList_ []),Raw_ ("abc def ")]                       