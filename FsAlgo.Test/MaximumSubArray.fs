namespace FsAlgo.Test

open Expecto
open FsCheck
open FsAlgo.MaximumSubarray

module MaximumSubArray =

    [<Tests>]
    let maximumSubarrayTests =
        testList "maximumSubArray" [
            testList "maximumSubArrayDivideAndConquer" [
                test "max subarray crosses middle" {

                    // 10 - 1 + 3 + 2 is the maximum subarray with sum 14, left index 3 and right index 6
                    let a = [-1.; 2.; -3.; 10.; -1.; 3.; 2.; -2.; -7.; 8.]
                    let minIdx, maxIdx, s = divideAndConquer a
                    Expect.equal minIdx 3 "Max (10) is at index 3"
                    Expect.equal maxIdx 6 "2 ends the maximum subarray and is at index 6"
                    Expect.equal s 14. "The sum of 10-1+3+2 is 14"
                }

                test "max subarray is only the first element" {
                    let a = [10.; -9.; -3.; -2.5; 5.; 4.]
                    let minIdx, maxIdx, s = divideAndConquer a
                    Expect.equal minIdx 0 "Max (10) is at index 0"
                    Expect.equal maxIdx 0 "The subarray consists of only one element, and so ends at index 0"
                    Expect.equal s 10. "The element is 10, with only one element the sum is also 10"
                }

                test "input array only contains negative values" {
                    let a = [-5.; -3.; -2.; -10.;]
                    let minIdx, maxIdx, s = divideAndConquer a
                    Expect.equal minIdx 2 "Max (-2) is at index 2"
                    Expect.equal maxIdx 2 "The subarray consists of only one element, and so ends at index 2"
                    Expect.equal s -2. "The element is -2, with only one element the sum is also -2"
                }
            ];

            testList "maximumSubArrayLinear" [
                test "max subarray crosses middle" {

                    // 10 - 1 + 3 + 2 is the maximum subarray with sum 14, left index 3 and right index 6
                    let a = [-1.; 2.; -3.; 10.; -1.; 3.; 2.; -2.; -7.; 8.]
                    let minIdx, maxIdx, s = linearAlgo a
                    Expect.equal minIdx 3 "Max (10) is at index 3"
                    Expect.equal maxIdx 6 "2 ends the maximum subarray and is at index 6"
                    Expect.equal s 14. "The sum of 10-1+3+2 is 14"
                }

                test "max subarray is only the first element" {
                    let a = [10.; -9.; -3.; -2.5; 5.; 4.]
                    let minIdx, maxIdx, s = linearAlgo a
                    Expect.equal minIdx 0 "Max (10) is at index 0"
                    Expect.equal maxIdx 0 "The subarray consists of only one element, and so ends at index 0"
                    Expect.equal s 10. "The element is 10, with only one element the sum is also 10"
                }

                test "input array only contains negative values" {
                    let a = [-5.; -3.; -2.; -10.;]
                    let minIdx, maxIdx, s = linearAlgo a
                    Expect.equal minIdx 2 "Max (-2) is at index 2"
                    Expect.equal maxIdx 2 "The subarray consists of only one element, and so ends at index 2"
                    Expect.equal s -2. "The element is -2, with only one element the sum is also -2"
                }
            ]

            // For the equivalence property, we do not care about treatment of zeros, they can either be included or excluded.
            testProp "linear and divideAndConquer equivalence" <| fun (an: NonEmptyList<NonNanNonZeroFloat>) ->
                let a = an.Get |> List.map (fun f -> f.Get)

                let linearMinIdx, linearMaxIdx, linearSum = linearAlgo a
                let dMinIdx, dMaxIdx, dSum = divideAndConquer a
                Expect.equal linearMinIdx dMinIdx "Mininum index of subarray was different"
                Expect.equal linearMaxIdx dMaxIdx "Maximum index of subarray was different"
                Expect.floatClose Accuracy.high linearSum dSum "Sum of subarray was different"
        ]