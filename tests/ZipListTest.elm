module ZipListTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import ZipList exposing (ZipList)


suite : Test
suite =
    describe "The ZipList module"
        [ fuzz (list int) "The toList function" <|
            \randomList ->
                Expect.equal randomList (ZipList.init randomList |> ZipList.toList)
        , describe "The current function"
            [ test "An empty list should return Nothing" <|
                \_ ->
                    Expect.equal Nothing (ZipList.init [] |> ZipList.current)
            , test "A non empty list" <|
                \_ ->
                    Expect.equal (Just 1) (ZipList.init [ 1, 2 ] |> ZipList.current)
            ]
        , describe "The next function" <|
            [ test "An empty list" <|
                \_ ->
                    moveTest ZipList.next [] Nothing
            , test "already at the list end" <|
                \_ ->
                    moveTest ZipList.next [ 1 ] (Just 1)
            , test "still some moves to make" <|
                \_ ->
                    moveTest ZipList.next [ 1, 2 ] (Just 2)
            ]
        , describe "The previous function" <|
            [ test "An empty list" <|
                \_ ->
                    moveTest ZipList.previous [] Nothing
            , test "already at the list beginning" <|
                \_ ->
                    moveTest ZipList.previous [ 1 ] (Just 1)
            , test "still some moves to make" <|
                \_ ->
                    moveTest (ZipList.next >> ZipList.previous) [ 1, 2 ] (Just 1)
            ]
        , fuzz (list int) "The length function" <|
            \randomList ->
                Expect.equal (List.length randomList) (ZipList.length <| ZipList.init randomList)
        ]


moveTest : (ZipList a -> ZipList a) -> List a -> Maybe a -> Expectation
moveTest moveFunc list expectedValue =
    let
        zipList =
            ZipList.init list

        nextZipList =
            moveFunc zipList
    in
        Expect.equal expectedValue (ZipList.current nextZipList)
