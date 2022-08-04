namespace FsAlgo

open System

[<AutoOpen>]
module Constants =

    /// Float epsilon.
    /// The smallest representable change from zero.
    let eps = Double.Epsilon

    /// Calculates machine epsilon valid on the executing machine,
    /// representing the upper bound of the relative error due to rounding in floating-point arithmetic.
    let private calcMachEps =
        let rec inner e = if e + 1.0 <> 1.0 then inner (e/2.) else e
        inner 1e-4

    /// Calculated machine epsilon
    let machEps = calcMachEps

    type Ordering =
        | Smaller = -1
        | Equal = 0
        | Greater = 1

    /// Compares two elements,
    /// Greater (1) if x > y
    /// Equal (0) if x = y
    /// Smaller (-1) if y > x
    let inline cmp x y =
        if x > y then
            Ordering.Greater
        elif x = y then
            Ordering.Equal
        else
            Ordering.Smaller

    // Returns odering indicator based on values and sort ascending or descending preference.
    // E.g. for x > y, then Ordering.Smaller is returned if sort descending is true.
    let inline sortOrder sortAsc (x: 'a) (y: 'a) =
        match sortAsc with
        | ValueSome s -> if s then cmp x y else cmp y x
        | ValueNone -> cmp x y
