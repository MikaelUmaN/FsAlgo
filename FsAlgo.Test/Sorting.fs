namespace FsAlgo.Test

open Expecto
open FsCheck
open FsAlgo.Sorting

module Sorting =

    [<Tests>]
    let sortingTests =
        testList "sort" [
            testList "selection sort" [
                testProp "F# list sort equivalence (asc)" <| fun (an: NonNanFloat list) ->
                    let a = an |> List.map float
                    let expected = a |> List.sort
                    let actual = selectionSort a ValueNone
                    Expect.equal actual expected "Expected sort to yield same order as built-in sort"

                testProp "F# list sort equivalence (desc)" <| fun (an: NonNanFloat list) ->
                    let a = an |> List.map float
                    let expected = a |> List.sortDescending
                    let actual = selectionSort a (ValueSome false)
                    Expect.equal actual expected "Expected sort to yield same order as built-in sort"
            ]

            testList "merge sort" [
                testProp "F# list sort equivalence (asc)" <| fun (an: NonNanFloat list) ->
                    let a = an |> List.map float
                    let expected = a |> List.sort
                    let actual = mergeSort a ValueNone
                    Expect.equal actual expected "Expected sort to yield same order as built-in sort"

                testProp "F# list sort equivalence (desc)" <| fun (an: NonNanFloat list) ->
                    let a = an |> List.map float
                    let expected = a |> List.sortDescending
                    let actual = mergeSort a (ValueSome false)
                    Expect.equal actual expected "Expected sort to yield same order as built-in sort"
            ]            
        ]
