namespace FsAlgo

/// Chapter 4 in Algorithms.
module MaximumSubarray =

    let private findMaxValueIndex (a: float list) =
        a
        |> List.mapi (fun i v -> i, v)
        |> List.maxBy snd

    let linearAlgo (a: float list) =
        let f (maxSum, minIdx, maxIdx, runningSum, runMinIdx, _) (n, nIdx) =
            
            // Update the running sum, taken into account the new element.
            let runningSumn, rMinIdx, rMaxIdx = 
                match runningSum with
                | ValueSome rs -> ValueSome(rs + n), runMinIdx, ValueSome nIdx
                | ValueNone when n > 0. -> ValueSome n, ValueSome nIdx, ValueSome nIdx
                | _ -> ValueNone, ValueNone, ValueNone

            match runningSumn with
            | ValueSome rs when rs < 0. -> 
                // This means that the current max subarray can never be extended past its current indices.
                // Reset the running sum.
                maxSum, minIdx, maxIdx, ValueNone, ValueNone, ValueNone
            | ValueSome rs ->
                // Else, the current max subarray can still be extended to include new elements, regardless
                // of whether the last delta was positive or negative.
                if rs > maxSum then
                    // Either the max sum has been extended or a new running sum has become the max sum.
                    rs, rMinIdx, rMaxIdx, runningSumn, rMinIdx, rMaxIdx
                else
                    maxSum, minIdx, maxIdx, runningSumn, rMinIdx, rMaxIdx
            | ValueNone ->
                // Else we don't have a new running sum yet.
                maxSum, minIdx, maxIdx, ValueNone, ValueNone, ValueNone

        let aIdx = List.zip a [ 0 .. a.Length-1 ]
        let maxSum, minIdx, maxIdx, _, _, _ = List.fold f (-infinity, ValueNone, ValueNone, ValueNone, ValueNone, ValueNone) aIdx
        match minIdx with
        | ValueNone ->
            // Special case when all elements are negative.
            let maxValIdx, maxVal = findMaxValueIndex a
            maxValIdx, maxValIdx, maxVal
        | _ ->
            minIdx.Value, maxIdx.Value, maxSum

    /// Divide and conquer algorithm.
    let divideAndConquer (a: float list) = 
        
        let rec inner (a: float list) minIdx maxIdx =

            /// Algorithm for finding the maximum sub array that crosses the middle.
            let findMaximumCrossingSubArray (a: float list) midIdx minIdx maxIdx =
                let m = a.Length / 2
                let leftList = List.zip a[..m-1] [ minIdx .. midIdx-1 ]
                let rightList = List.zip a[m..] [ midIdx .. maxIdx ]

                let leftIdx, _, leftSum =
                    List.foldBack
                        (fun (n, nIx) (ix, s, ls) ->
                            if n + s > ls then
                                nIx, n + s, n + s
                            else
                                ix, n + s, ls)
                        leftList
                        (midIdx-1, 0., -infinity)

                let rightIdx, _, rightSum =
                    List.fold
                        (fun (ix, s, rs) (n, nIx) ->
                            if n + s > rs then
                                nIx, n + s, n + s
                            else
                                ix, n + s, rs)
                        (midIdx, 0., -infinity)
                        rightList

                leftIdx, rightIdx, leftSum + rightSum

            // Base case of one element.
            if a.Length = 1 then
                minIdx, maxIdx, a[0]
            else
                let m = a.Length / 2
                let mIdx = m + minIdx
                let leftMinIdx, leftMaxIdx, leftSum = inner a[..m-1] minIdx (mIdx-1)

                let rightMinIdx, rightMaxIdx, rightSum =
                    inner a[m..] mIdx  maxIdx

                let crossMinIdx, crossMaxIdx, crossSum =
                    findMaximumCrossingSubArray a mIdx minIdx maxIdx

                // Either the left array contains the maximum subarray,
                // or the right array contains it,
                // or else the maximum subarray crosses the middle.
                if leftSum >= rightSum && leftSum >= crossSum then
                    leftMinIdx, leftMaxIdx, leftSum
                elif rightSum >= leftSum && rightSum >= crossSum then
                    rightMinIdx, rightMaxIdx, rightSum
                else
                    crossMinIdx, crossMaxIdx, crossSum
            
        if a |> List.forall (fun n -> n <= 0.) then
            // Special case when no element is positive.
            let maxValIdx, maxVal = findMaxValueIndex a
            maxValIdx, maxValIdx, maxVal
        else
            inner a 0 (a.Length-1)