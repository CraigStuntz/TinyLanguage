module Binding

open Syntax

type BindingType =
    | IntType
    | BoolType
    | StringType
    | VoidType
    | FunctionType of (BindingType list) * BindingType
    | ErrorType of string

let rec prettyPrintType = function 
| IntType     -> "integer"
| BoolType    -> "boolean"
| StringType  -> "string"
| VoidType    -> "void"
| FunctionType (argumentTypes, resultType) -> 
    sprintf "%s -> %s" (argumentTypes |> List.map prettyPrintType |> String.concat ", ") (prettyPrintType resultType)
| ErrorType message -> 
    sprintf "Error (%s)" message

type ArgumentBinding = {
    Name: string
    ArgumentType: BindingType
}

type Function = {
    Name:       string
    Arguments:  ArgumentBinding list
    Body:       Binding
    ResultType: BindingType
}
and Invocation = {
    Name: string
    Arguments: Binding list
    ResultType: BindingType
}
and Statement =
    | Defun  of Function
    | Ignore of Binding
and Binding = 
    | BoolBinding     of bool
    | IntBinding      of int 
    | StringBinding   of string
    | InvokeBinding   of Invocation
    | FunctionBinding of Function
    | ErrorBinding    of string

let rec prettyPrintBinding = function
| BoolBinding     value -> sprintf "%A" value
| IntBinding      value -> sprintf "%d" value
| StringBinding   value -> sprintf "%s" value
| InvokeBinding   value -> sprintf "(%s %s)" value.Name (value.Arguments |> List.map prettyPrintBinding |> String.concat " ")
| FunctionBinding value -> value.Name
| ErrorBinding  message -> sprintf "Error (%s)" message

let rec inferType (binding: Binding) : BindingType =
    match binding with
    | BoolBinding   _ -> BoolType
    | IntBinding    _ -> IntType
    | StringBinding _ -> StringType
    | FunctionBinding { Name = name; Arguments = argBindings; Body = bodyBinding; ResultType = resultType } ->
        FunctionType (argBindings |> List.map (fun { ArgumentType = argType } -> argType), resultType)
    | InvokeBinding { Arguments = arguments; ResultType = resultType} ->
        let argTypes = arguments |> List.map inferType
        FunctionType (argTypes, resultType)
    | ErrorBinding message -> ErrorType message

let private findArgumentError (invokedArgument: Binding, functionArgument: ArgumentBinding) =
    match inferType invokedArgument = functionArgument.ArgumentType with
    | true  -> None
    | false -> Some (sprintf "Expected %s; found %A." (functionArgument.ArgumentType |> prettyPrintType) (invokedArgument |> prettyPrintBinding) )

let private findArgumentErrors (invokedArguments: Binding list, functionArguments: ArgumentBinding list) =
    match List.length invokedArguments = List.length functionArguments with
    | false -> Some (ErrorBinding (sprintf "Expected %d arguments, found %d." (List.length functionArguments) (List.length invokedArguments)))
    | true ->
        match (List.zip invokedArguments functionArguments) |> List.choose findArgumentError with
        | [] -> None
        | errors -> Some (ErrorBinding (errors |> String.concat System.Environment.NewLine))

let rec private toBinding (environment: Map<string, Binding>) (expression : Expression) : Binding = 
    match expression with
    | IntExpr n                         -> IntBinding n
    | StringExpr str                    -> StringBinding str
    | DefunExpr _                       -> failwith "DefunExpr should be hanlded by fromExpression and isn't allowed here"
    | InvokeExpr (name, exprs)          -> 
        match environment.TryFind name with
        | Some (FunctionBinding func) -> 
            let invokedArguments = exprs |> List.map (toBinding environment)
            match findArgumentErrors(invokedArguments, func.Arguments) with
            | Some errorBinding -> errorBinding
            | None -> 
                InvokeBinding { 
                    Name = name
                    Arguments = invokedArguments
                    ResultType = 
                        match environment.[name] |> inferType with
                        | FunctionType (argTypes, resultType) -> resultType
                        | otherType -> otherType
                }
        | Some bindingType -> ErrorBinding (sprintf "Expected function, found %A" bindingType)
        | None -> ErrorBinding (sprintf "Undefined function '%s'." name)
    | ErrorExpr error                   -> ErrorBinding error
    
let rec fromExpression (environment: Map<string, Binding>) (expression : Expression) : Statement = 
    match expression with
    | DefunExpr (name, arguments, body) ->  
        let toArgument argName = { Name = argName; ArgumentType = environment.[argName] |> inferType}
        let argBindings = arguments |> List.map toArgument
        let bodyBinding = toBinding environment body
        Defun { Name = name; Arguments = argBindings; Body = bodyBinding; ResultType = inferType bodyBinding }
    | _ -> 
        Ignore (toBinding environment expression)

let private builtins: Map<string, Binding> = 
    [
        ("+", FunctionBinding { 
            Name = "+"; 
            Arguments = [ { Name = "first"; ArgumentType = IntType }; { Name = "second"; ArgumentType = IntType } ];    
            Body = IntBinding 0; 
            ResultType = IntType 
        })
    ] |> Map.ofList
 
let fromExpressions = List.map (fromExpression builtins)