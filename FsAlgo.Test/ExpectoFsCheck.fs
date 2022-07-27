namespace FsAlgo.Test

open FsAlgo
open System
open Expecto
open FsCheck

[<AutoOpen>]
module ExpectoFsCheck =

    let filterSmallFloatsExclZero f = abs f > machEps || f = 0.
    let filterSmallFloats f = abs f > machEps

    let nonNegInt = Arb.generate<NonNegativeInt>

    type NonNanNonZeroFloat = NonNanNonZeroFloat of float with
        member x.Get = match x with NonNanNonZeroFloat f -> f
        override x.ToString() = x.Get.ToString()
        static member op_Explicit(NormalFloat f) = f

    let nonNanFloat =
        Arb.generate<NormalFloat>
        |> Gen.filter (fun (NormalFloat f) -> filterSmallFloatsExclZero f)

    let nonNanNonZeroFloat = 
        Arb.generate<NormalFloat>
        |> Gen.filter (fun (NormalFloat f) -> filterSmallFloats f)
        |> Gen.map (fun (NormalFloat f) -> NonNanNonZeroFloat(f))

    type FloatGens =
        static member NonNanNonZeroFloat() =
            { new Arbitrary<NonNanNonZeroFloat>() with
                override x.Generator = nonNanNonZeroFloat
            }

    ///Represents a non-empty list.
    type NonEmptyList<'a> = NonEmptyList of 'a list with
        member x.Get = match x with NonEmptyList f -> f
        override x.ToString() = x.Get.ToString()

    let nonEmptyListGen<'a> : Gen<NonEmptyList<'a>> = 
        gen { 
            let! an = Arb.generate<NonEmptyArray<'a>>
            let a = an.Get |> Array.toList
            return NonEmptyList a
        }

    type ListGens =
        static member NonEmptyList() =
            Arb.fromGen nonEmptyListGen

    let private config = { 
        FsCheckConfig.defaultConfig with 
            maxTest = 1000
            arbitrary = [typeof<ListGens>; typeof<FloatGens>]
    }
    
    let testProp name = testPropertyWithConfig config name