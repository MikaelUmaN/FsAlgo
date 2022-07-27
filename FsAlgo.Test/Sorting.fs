namespace FsAlgo.Test

open Expecto
open FsCheck
open FsAlgo.Sorting

module Sorting =

    [<Tests>]
    let sortingTests =
        testList "sort" [
            testList "bubble" [
                testProp "F# list sort equivalence" <| fun (a: NormalFloat list) ->
                    let expected = a |> List.sort
                    let actual = bubbleSort a ValueNone
                    Expect.equal actual expected "Expected bubble sort to yield same order as built-in sort"
            ]
        ]
