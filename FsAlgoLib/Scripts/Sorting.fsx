#I "/home/jovyan/work/FsAlgo/FsAlgoLib/bin/Debug/net6.0/"
#r "FsAlgoLib.dll"

let a = [10; -3; 2; 1; 20; -20; 3; 2; 24]

open FsAlgo

let b = mergeSort a ValueNone
let c =  mergeSort a (ValueSome false)


let qb = quickSort a ValueNone
let qc = quickSort a (ValueSome false)