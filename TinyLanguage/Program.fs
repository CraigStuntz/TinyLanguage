open Railway

[<EntryPoint>]
let main argv = 
    match Compiler.compile "(+ 1 (+ 2 3 4))" with
    | Success assemblyBuilder -> 
        assemblyBuilder.Save("test.exe")
        printf "good"
    | Failure errorMessages -> eprintf "Failed. %s" errorMessages

    System.Console.ReadLine() |> ignore

    printfn "%A" argv
    0 // return an integer exit code
