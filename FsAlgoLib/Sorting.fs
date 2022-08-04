namespace FsAlgo

[<AutoOpen>]
module Sorting =

    /// Standard sorting, comparable to bubble sort and insertion sort. Runtime O(n^2).
    let selectionSort (a: 'a list) (sortAsc: bool voption) =
        let sOrder = sortOrder sortAsc

        // Selection algorithm, traverses the list.
        // Picks the largest element.
        let pick a = 
            let rec inner (a: 'a list) (an: 'a voption) b =
                match a with
                | x::xs ->
                    match an with
                    | ValueSome ann ->
                        let indic = sOrder x ann
                        match indic with
                        | Ordering.Greater -> inner xs (ValueSome x) (ann::b)
                        | _ -> inner xs an (x::b)
                    | ValueNone -> inner xs (ValueSome x) b
                | [] -> an, b

            let an, b = inner a ValueNone []
            an.Value, b

        // Recursively select the next element to put at the end.
        let rec inner a b =
            match a with
            | [] -> b
            | xs -> 
                let bn, xsn = pick xs
                inner xsn (bn::b)

        inner a []

    let mergeSort (a: 'a list) (sortAsc: bool voption) =
        let sOrder = sortOrder sortAsc

        let merge a b =
            let rec inner a b m =
                match a, b with
                | x::xs, y::ys ->
                    match sOrder x y with
                    | Ordering.Smaller -> inner xs b (x::m)
                    | _ -> inner a ys (y::m)
                | xs, [] ->
                    List.rev m @ xs
                | [], ys ->
                    List.rev m @ ys
            inner a b []

        let rec divide a =
            match a with
            | [] -> []
            | x::[] -> [x]
            | xs ->
                let d = xs.Length / 2
                let x = divide xs[..d-1]
                let y = divide xs[d..]
                merge x y

        divide a

    let inline private (|Median|) (x, y, z) =
        if (x >= y && x <= z) || (x >= z && x <= y) then
            x
        elif (y >= x && y <= z) || (y >= z && y <= x) then
            y
        else
            z

    let quickSort (a: 'a list) (sortAsc: bool voption) =
        let rec divide (a: 'a list) =
            let firstVal, middleVal, lastVal = a[0], a[a.Length/2], a[^1]
            match firstVal, middleVal, lastVal with
            | Median(pivot) -> 
                let ls, gt = List.fold (fun (ls, gt) x -> if x >= pivot then ls, x::gt else x::ls, gt) ([], []) a
                let lsSorted = if ls.Length < 4 then selectionSort ls sortAsc else divide ls
                let gtSorted = if gt.Length < 4 then selectionSort gt sortAsc else divide gt
                match sortAsc with
                | ValueSome s when not s -> gtSorted @ lsSorted
                | _ -> lsSorted @ gtSorted

        if a.Length < 4 then
            selectionSort a sortAsc
        else
            divide a

module ImperativeSorting =

    /// Swaps values at indices i and j.
    let inline swap (a: 'a []) i j = 
        let t = a[i]
        a[i] <- a[j]
        a[j] <- t

    let selectionSort (a: 'a []) (sortAsc: bool voption) =
        if a.Length < 2 then
            a
        else
            let sOrder = sortOrder sortAsc

            let n = a.Length
            for i in n-1.. -1 .. 1 do
                let mutable cIdx = 0
                for j in 1..i do
                    // The largest elements are placed at the end.
                    if sOrder a[j] a[cIdx] = Ordering.Greater then
                        cIdx <- j
                    
                if i <> cIdx then
                    swap a i cIdx
            a

    // Eval how much overhead value options entail.
    let selectionSortAsc (a: 'a []) =
        if a.Length < 2 then
            a
        else
            let n = a.Length
            for i in n-1.. -1 .. 1 do
                let mutable cIdx = 0
                for j in 1..i do
                    // The largest elements are placed at the end.
                    if a[j] > a[cIdx] then
                        cIdx <- j
                    
                if i <> cIdx then
                    swap a i cIdx
            a