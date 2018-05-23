open Railway

[<EntryPoint>]
let main argv = 
    match Compiler.compile "(defun main () (inc (inc 2)))" with
    | Success assemblyBuilder -> 
        let outfileName = "test.exe"
        assemblyBuilder.Save(outfileName)
        System.Diagnostics.Process.Start(outfileName) |> ignore
        printf "Successfully compiled and executed %s" outfileName
    | Failure errorMessages -> eprintf "Failed. %s" errorMessages

    System.Console.ReadLine() |> ignore

    printfn "%A" argv
    0 // return an integer exit code
