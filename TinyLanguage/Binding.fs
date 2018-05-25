module Binding

type Type =
    | IntType
    | BoolType
    | StringType
    | FunctionType of Type option * Type
    | ErrorType of string

type ArgumentBinding = {
    ArgumentName: string
    ArgumentType: Type
}

type Function = 
    | UserFunction of Argument: ArgumentBinding option * Body: Binding * ResultType: Type
    | Inc          
and Invocation = {
    FunctionName: string
    Function:     Function
    Argument:     Binding option
}
and Def = {
    VariableName:    string
    VariableBinding: Binding
    Body:            Binding
}
and Binding = 
    | BoolBinding     of bool
    | IntBinding      of int 
    | StringBinding   of string
    | VariableBinding of variableName: string * variableType: Type
    | InvokeBinding   of Invocation
    | FunctionBinding of Function
    | IncBinding      of Binding   // Eventually replace this with a list of builtins
    | DefBinding      of Def
    | ErrorBinding    of string


let rec prettyPrintType = function 
| IntType     -> "int"
| BoolType    -> "bool"
| StringType  -> "string"
| FunctionType (argument, resultType) -> 
    match argument with
    | Some argumentBindingType ->
        sprintf "%s -> %s" (prettyPrintType argumentBindingType) (prettyPrintType resultType)
    | None ->
        sprintf "-> %s" (prettyPrintType resultType)
| ErrorType message -> 
    sprintf "Error (%s)" message


let rec prettyPrintBinding = function
| BoolBinding     value  -> sprintf "%A" value
| IntBinding      value  -> sprintf "%d" value
| StringBinding   value  -> sprintf "%s" value
| VariableBinding (variableName = name)  -> name
| IncBinding binding     -> 
    sprintf "(inc %s)" (binding |> prettyPrintBinding)
| InvokeBinding   value  -> 
    match value.Argument with
    | Some argument -> 
        sprintf "(%s %s)" value.FunctionName (argument |> prettyPrintBinding)
    | None -> 
        sprintf "(%s)" value.FunctionName
| FunctionBinding value  -> 
    match value with
    | UserFunction (argument, body, resultType) ->
        match argument with
        | Some arg -> sprintf "(lambda %s %s)" arg.ArgumentName (prettyPrintBinding body)
        | None     -> sprintf "(lambda () %s)" (prettyPrintBinding body)
    | Inc -> "inc"
| DefBinding      value  ->
    match value.Body with
    | FunctionBinding (UserFunction (argument, body, resultType)) -> 
        match argument with
        | Some arg -> 
            sprintf "(defun %s (%s %s) %s)" 
                value.VariableName (prettyPrintType arg.ArgumentType) arg.ArgumentName (prettyPrintBinding body)
        | None  -> 
            sprintf "(defun %s () %s)" 
                value.VariableName (prettyPrintBinding body)    
    | _  -> sprintf "(def %s %s)" value.VariableName (prettyPrintBinding value.Body)
| ErrorBinding message -> sprintf "Error (%s)" message

