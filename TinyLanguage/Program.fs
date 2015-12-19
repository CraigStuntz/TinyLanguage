open Railway

[<EntryPoint>]
let main argv = 
    match Compiler.compile "(defun main () (inc (inc 2)))" with
    | Success assemblyBuilder -> 
        assemblyBuilder.Save("test.exe")
        printf "good"
    | Failure errorMessages -> eprintf "Failed. %s" errorMessages

    System.Console.ReadLine() |> ignore

    printfn "%A" argv
    0 // return an integer exit code
