module HtmlRunnerExample exposing (..)

{-| HOW TO RUN THIS EXAMPLE

$ elm-reactor

Visit http://localhost:8000 and bring up this file.
-}

import String
import Expect
import Test exposing (..)
import Test.Runner.Html
import Fuzz exposing (Fuzzer, int, string)
import Random.Pcg as Random
import Shrink


{-| A fuzzzer that usually generates "foo", but occasonally "bar". We expect a claim that it's always "foo" to fail.
-}
usuallyFoo : Fuzzer String
usuallyFoo =
    Fuzzer
        (Random.oneIn 30
            |> Random.map
                (\b ->
                    if b then
                        "bar"
                    else
                        "foo"
                )
        )
        Shrink.string


actualFuzzTest : Test
actualFuzzTest =
    describe "actual fuzz test"
        [ fuzz usuallyFoo "description goes here" <|
            \shouldBeFoo ->
                shouldBeFoo
                    |> Expect.equal "foo"
                    |> Expect.onFail "It wasn't \"foo\"."
        ]


main : Program Never
main =
    [ testOxfordify
    , noDescription
    , testExpectations
    , testFailingFuzzTests
    , actualFuzzTest
    , testFuzz
    , testShrinkables
    ]
        |> batch
        |> Test.Runner.Html.run


testExpectations : Test
testExpectations =
    describe "basic expectations"
        [ test "this should succeed" <|
            \() ->
                "blah"
                    |> Expect.equal " blah"
        , test "this should fail" <|
            \() ->
                "something"
                    |> Expect.equal "someting else"
        , test "another failure" <|
            \() ->
                "forty-two"
                    |> Expect.equal "forty-three"
        ]



{- After this point, we're really just showing that Richard's proposed API compiles. -}


{-| stubbed function under test
-}
oxfordify : a -> b -> c -> String
oxfordify _ _ _ =
    "Alice, Bob, and Claire"


{-| Stubbed fuzzer - TODO implement
-}
string : Fuzzer String
string =
    Fuzzer (Random.choice "foo" "bar")
        Shrink.string


noDescription : Test
noDescription =
    test "" <|
        \() ->
            Expect.equal "No description" "Whatsoever!"


testFuzz : Test
testFuzz =
    describe "fuzzing"
        [ fuzz2 string string "empty list etc" <|
            \name punctuation ->
                oxfordify "This sentence is empty" "." []
                    |> Expect.equal ""
                    |> Expect.onFail "given an empty list, did not return an empty string"
        , fuzz2 string string "further testing" <|
            \name punctuation ->
                oxfordify "This sentence contains " "." [ "one item" ]
                    |> Expect.equal "This sentence contains one item."
        , fuzz2 string string "custom onFail here" <|
            \name punctuation ->
                oxfordify "This sentence contains " "." [ "one item", "two item" ]
                    |> Expect.equal "This sentence contains one item and two item."
                    |> Expect.onFail "given an empty list, did not return an empty string"
        , fuzz2 string string "This is a test." <|
            \name punctuation ->
                oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
                    |> Expect.equal "This sentence contains one item, two item, and three item."
                    |> Expect.onFail "given a list of length 3, did not return an oxford-style sentence"
        ]


testFailingFuzzTests : Test
testFailingFuzzTests =
    describe "the first element in this fuzz tuple"
        [ fuzz2 string string "is always \"foo\"" <|
            \str1 str2 ->
                str1
                    |> Expect.equal "foo"
        ]


testOxfordify : Test
testOxfordify =
    describe "oxfordify"
        [ describe "given an empty sentence"
            [ test "returns an empty string" <|
                \() ->
                    oxfordify "This sentence is empty" "." []
                        |> Expect.equal ""
            ]
        , describe "given a sentence with one item"
            [ test "still contains one item" <|
                \() ->
                    oxfordify "This sentence contains " "." [ "one item" ]
                        |> Expect.equal "This sentence contains one item."
            ]
        , describe "given a sentence with multiple items"
            [ test "returns an oxford-style sentence" <|
                \() ->
                    oxfordify "This sentence contains " "." [ "one item", "two item" ]
                        |> Expect.equal "This sentence contains one item and two item."
            , test "returns an oxford-style sentence" <|
                \() ->
                    oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
                        |> Expect.equal "This sentence contains one item, two item, and three item."
            ]
        ]


testShrinkables : Test
testShrinkables =
    describe "Some tests that should fail and produce shrunken values"
        [ describe "a randomly generated integer"
            [ fuzz int "is for sure exactly 0" <| Expect.equal 0
            , fuzz int "is <42" <| Expect.lessThan 42
            , fuzz int "is also >42" <| Expect.greaterThan 42
            ]
        , describe "a randomly generated string"
            [ fuzz string "equals its reverse" <|
                \str ->
                    Expect.equal str (String.reverse str)
            ]
        ]
