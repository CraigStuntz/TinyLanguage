module Context

open Binding

type Context = Map<string, Binding>


let extend (environment: Context) (name: string, bindingType: Type) : Context =
    environment.Add(name, VariableBinding(variableName = name, variableType = bindingType))


/// Returns an environment containing the "intrinsic" functions known by the compiler
/// "Intrinsics" are the functions that the compiler knows an implementation for; there
/// doesn't need to be any source code provided because the compiler cna produce the IL
/// needed. In the case of this simple compiler, it's just "inc"
let intrinsics: Context = 
    [
        ("inc", FunctionBinding Inc)
    ] |> Map.ofList