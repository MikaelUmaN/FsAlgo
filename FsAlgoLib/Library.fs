namespace FsAlgoLib

/// Chapter 4 in Algorithms.
module MaximumSubarray =

    /// Divide and conquer algorithm.
    let rec maxSubArrayDivideAndConquer (a: float list) minIdx maxIdx =

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
            let leftMinIdx, leftMaxIdx, leftSum = maxSubArrayDivideAndConquer a[..m-1] minIdx (mIdx-1)

            let rightMinIdx, rightMaxIdx, rightSum =
                maxSubArrayDivideAndConquer a[m..] mIdx  maxIdx

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
