open Expecto

module Program =

    [<EntryPoint>]
    let main args =
    
        // Normal run
        runTestsInAssemblyWithCLIArgs [] args
