#I "/home/jovyan/work/FsAlgo/FsAlgoLib/bin/Debug/net6.0/"
#r "FsAlgoLib.dll"

open FsAlgoLib.MaximumSubarray

// 10 - 1 + 3 + 2 is the maximum subarray with sum 14, left index 3 and right index 6
let a = [-1.; 2.; -3.; 10.; -1.; 3.; 2.; -2.; -7.; 8.]

let minIdx, maxIdx, s = maxSubArrayDivideAndConquer a 0 (a.Length-1)