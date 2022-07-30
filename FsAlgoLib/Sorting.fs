namespace FsAlgo

module Sorting =

    type Ordering =
        | Smaller = -1
        | Equal = 0
        | Greater = 1

    /// Compares two elements,
    /// Greater (1) if x > y
    /// Equal (0) if x = y
    /// Smaller (-1) if y > x
    let cmp x y =
        if x > y then
            Ordering.Greater
        elif x = y then
            Ordering.Equal
        else
            Ordering.Smaller

    // Returns odering indicator based on values and sort ascending or descending preference.
    // E.g. for x > y, then Ordering.Smaller is returned if sort descending is true.
    let private sortOrder sortAsc x y =
        match sortAsc with
        | ValueSome s -> if s then cmp x y else cmp y x
        | ValueNone -> cmp x y

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