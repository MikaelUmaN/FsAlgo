namespace FsAlgo

module Sorting =

    type Ordering =
        | Smaller = -1
        | Equal = 0
        | Greater = 1

    /// Compares two elements,
    /// 1 if x > y
    /// 0 if x = y
    /// -1 if y > x
    let cmp x y =
        if x > y then
            Ordering.Greater
        elif x = y then
            Ordering.Equal
        else
            Ordering.Smaller

    /// Benchmark of sorting algorithms: bubble/sinking sort.
    /// Default sorting is ascending.
    /// Average and worst case running time is O(n^2).
    let bubbleSort (a: 'a list) (sortAsc: bool voption) =
        let asc = 
            match sortAsc with
            | ValueSome v -> v
            | ValueNone -> true

        // Because prepend is fast and default ordering is ascending,
        // lesser elements are placed first, i.e. 3::5::[8; 9]
        let rec inner (a: 'a list) (b: 'a list) i chg =
            let x = a[i]
            let y = a[i+1]
            let indic = cmp a[i] a[i+1]
            let xn, yn, chgn =
                match indic with
                | Ordering.Equal -> x, y, false
                | Ordering.Greater -> y, x, true
                | _ -> x, y, false

            let chgnn = chg || chgn
            let bn = xn::yn::b
            if i+1 = a.Length-1 then
                if chgnn then inner bn [] 0 false else bn
            else
                inner a bn (i+1) chgnn

        if a.Length < 2 then
            // Special case for empty or one-element lists.
            a
        else
            let aSorted = inner a [] 0 false
            if asc then aSorted else aSorted |> List.rev
