module Binding

type Type =
    | IntType
    | BoolType
    | StringType
    | FunctionType of Type * Type
    | UnitType
    | ErrorType of string

let parseTypeName (name: string) = 
    match name with
    | "int"    -> IntType
    | "bool"   -> BoolType
    | "string" -> StringType
    | "()"     -> UnitType
    | wrong    -> ErrorType (sprintf "Expected argument type; found '%s'." wrong)


let rec prettyPrintType = function 
| IntType     -> "int"
| BoolType    -> "bool"
| StringType  -> "string"
| FunctionType (argument, resultType) -> 
    sprintf "%s -> %s" (prettyPrintType argument) (prettyPrintType resultType)
| UnitType    -> "()"
| ErrorType message -> 
    sprintf "Error (%s)" message


type Argument = {
    ArgumentName: string
    ArgumentType: Type
}

type Function = 
    | UserFunction of Argument: Argument * Body: Binding * ResultType: Type
    | Inc          
and Invocation = {
    FunctionName: string
    Function:     Function
    Argument:     Binding
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
    | NilBinding
    | ErrorBinding    of string


let rec prettyPrintBinding = function
| BoolBinding     value  -> sprintf "%A" value
| IntBinding      value  -> sprintf "%d" value
| NilBinding             -> "()"
| StringBinding   value  -> sprintf "%s" value
| VariableBinding (variableName = name)  -> name
| IncBinding binding     -> 
    sprintf "(inc %s)" (binding |> prettyPrintBinding)
| InvokeBinding   value  -> 
    sprintf "(%s %s)" value.FunctionName (value.Argument |> prettyPrintBinding)
| FunctionBinding value  -> 
    match value with
    | UserFunction (argument, body, _resultType) ->
        match argument.ArgumentType with
        | UnitType -> sprintf "(lambda () %s)" (prettyPrintBinding body)
        | _        -> sprintf "(lambda %s %s)" argument.ArgumentName (prettyPrintBinding body)
    | Inc -> "inc"
| DefBinding      value  ->
    match value.Body with
    | FunctionBinding (UserFunction (argument, body, _resultType)) -> 
        match argument.ArgumentType with
        | UnitType -> 
            sprintf "(defun %s () %s)" 
                value.VariableName (prettyPrintBinding body)    
        | _ -> 
            sprintf "(defun %s (%s %s) %s)" 
                value.VariableName (prettyPrintType argument.ArgumentType) argument.ArgumentName (prettyPrintBinding body)

    | _  -> sprintf "(def %s %s)" value.VariableName (prettyPrintBinding value.Body)
| ErrorBinding message -> sprintf "Error (%s)" message

