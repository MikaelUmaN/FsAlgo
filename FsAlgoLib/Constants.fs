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