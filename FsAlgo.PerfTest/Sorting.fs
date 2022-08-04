namespace FsAlgo.PerfTest

[<AutoOpen>]
module Sorting =

    open FsAlgo
    open FsAlgo.Sorting
    open BenchmarkDotNet.Attributes

    [<RankColumn>]
    type Sort() =

        let mutable A: float list = []
        let mutable B: float[] = [||]

        [<Params(16, 32, 64, 128, 256, 512, 1024)>]
        member val N = 0 with get, set

        [<GlobalSetup>]
        member this.Setup() =
            let R = System.Random()
            A <- [for _ in 1..this.N -> R.NextDouble()]
            B <- A |> List.toArray

        [<Benchmark>]
        member this.FunctionalSelectionSort() =
            selectionSort A ValueNone

        [<Benchmark>]
        member this.ImperativeSelectionSort() =
            ImperativeSorting.selectionSort B ValueNone


        [<Benchmark>]
        member this.ImperativeSelectionSortAsc() =
            ImperativeSorting.selectionSortAsc B

        [<Benchmark>]
        member this.FunctionalMergeSort() =
            mergeSort A ValueNone

        [<Benchmark>]
        member this.FSharpSort() =
            List.sort A

        [<Benchmark>]
        member this.FSharpSortArray() =
            Array.sort B