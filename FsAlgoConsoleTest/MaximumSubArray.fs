namespace FsAlgoConsoleTest

open Expecto
open FsAlgoLib.MaximumSubarray

module MaximumSubArray =

    [<Tests>]
    let maximumSubarrayTests =
        testList "maximumSubarrayTests" [
            test "max subarray crosses middle" {

                // 10 - 1 + 3 + 2 is the maximum subarray with sum 14, left index 3 and right index 6
                let a = [-1.; 2.; -3.; 10.; -1.; 3.; 2.; -2.; -7.; 8.]
                let minIdx, maxIdx, s = maxSubArrayDivideAndConquer a 0 (a.Length-1)
                Expect.equal minIdx 3 "10 is at index 3"
                Expect.equal maxIdx 6 "2 ends the maximum subarray and is at index 6"
                Expect.equal s 14 "The sum of 10-1+3+2 is 14"
            }
        ]