#I "/home/jovyan/work/FsAlgo/FsAlgoLib/bin/Debug/net6.0/"
#r "FsAlgoLib.dll"

open FsAlgoLib.MaximumSubarray

// 10 - 1 + 3 + 2 is the maximum subarray with sum 14, left index 3 and right index 6
let a = [-1.; 2.; -3.; 10.; -1.; 3.; 2.; -2.; -7.; 8.]
let minIdx, maxIdx, s = divideAndConquer a

// Simple test with two positive numbers.
let b = [0.59; 0.]
let minIdxb, maxIdbx, sb = divideAndConquer b

let minIdxc, maxIdxc, sc = linearAlgo b

// Differs in treatment of zeros.
// Is the minimal subarray the best response?