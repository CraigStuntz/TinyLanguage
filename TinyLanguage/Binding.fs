module Binding

open Railway
open Syntax

type BindingType =
    | IntType
    | BoolType
    | StringType
    | VoidType
    | FunctionType of BindingType * BindingType
    | ErrorType of string

let rec prettyPrintType = function 
| IntType     -> "integer"
| BoolType    -> "boolean"
| StringType  -> "string"
| VoidType    -> "void"
| FunctionType (argumentType, resultType) -> 
    sprintf "%s -> %s" (prettyPrintType argumentType) (prettyPrintType resultType)
| ErrorType message -> 
    sprintf "Error (%s)" message

type ArgumentBinding = {
    Name: string
    ArgumentType: BindingType
}

type Function = {
    Name:       string
    Argument:   ArgumentBinding
    Body:       Binding
    ResultType: BindingType
}
and Invocation = {
    Name:       string
    Argument:   Binding
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
    | VarBinding      of string * BindingType
    | ErrorBinding    of string

let rec prettyPrintBinding = function
| BoolBinding     value  -> sprintf "%A" value
| IntBinding      value  -> sprintf "%d" value
| StringBinding   value  -> sprintf "%s" value
| InvokeBinding   value  -> sprintf "(%s %s)" value.Name (value.Argument |> prettyPrintBinding)
| FunctionBinding value  -> value.Name
| VarBinding (name, typ) -> sprintf "(%s: %A)" name typ
| ErrorBinding  message  -> sprintf "Error (%s)" message

let rec inferType (binding: Binding) : BindingType =
    match binding with
    | BoolBinding   _ -> BoolType
    | IntBinding    _ -> IntType
    | StringBinding _ -> StringType
    | FunctionBinding { Name = name; Argument = argBinding; Body = bodyBinding; ResultType = resultType } ->
        FunctionType (argBinding.ArgumentType, resultType)
    | InvokeBinding { Argument = argument; ResultType = resultType} ->
        let argType = argument |> inferType
        FunctionType (argType, resultType)
    | VarBinding (_, typ) -> typ
    | ErrorBinding message -> ErrorType message

let rec private tryBind (expression: Expression) : Binding option =
    match expression with
    | IntExpr    i -> Some (IntBinding i)
    | StringExpr s -> Some (StringBinding s)
    | _            -> None

let rec private toBinding (environment: Map<string, Binding>) (expression : Expression) : Binding * BindingType option =
    match expression with
    | IdentifierExpr name               -> ErrorBinding (sprintf "Unrecognized identifier '%s'." name), None
    | IntExpr n                         -> IntBinding n, None
    | StringExpr str                    -> StringBinding str, None
    | DefunExpr _                       -> failwith "DefunExpr should be hanlded by fromExpression and isn't allowed here"
    | InvokeExpr (name, argument)       -> 
        match environment.TryFind name with
        | Some (FunctionBinding func) -> 
            match (tryBind argument), func.Argument.ArgumentType with
            | (Some invokedBinding), definedType when (inferType invokedBinding) = definedType ->
                InvokeBinding { 
                    Name = name
                    Argument = invokedBinding
                    ResultType = 
                        match environment.[name] |> inferType with
                        | FunctionType (argTypes, resultType) -> resultType
                        | otherType -> otherType
                }, Some definedType
            | (Some invokedBinding), definedType ->
                ErrorBinding (sprintf "Expected %s; found %A." (definedType |> prettyPrintType) (invokedBinding |> prettyPrintBinding)), None
            | None, definedType -> 
                InvokeBinding { 
                    Name = name
                    Argument = VarBinding(func.Argument.Name, definedType)
                    ResultType = 
                        match environment.[name] |> inferType with
                        | FunctionType (argTypes, resultType) -> resultType
                        | otherType -> otherType
                }, Some definedType
        | Some bindingType -> ErrorBinding (sprintf "Expected function; found %A" bindingType), None
        | None -> ErrorBinding (sprintf "Undefined function '%s'." name), None
    | ErrorExpr error                   -> ErrorBinding error, None


type private BindingState = {
    Environment: Map<string, Binding>
    Statements: Statement list
}
 
let rec private fromExpression (bindingState : BindingState) (expression : Expression) : BindingState = 
    match expression with
    | DefunExpr (name, argument, body) when (not (bindingState.Environment.ContainsKey name)) ->  
        match body |> (toBinding bindingState.Environment) with
        | bodyBinding, Some argumentType ->
            let defun = { Name = name; Argument = { Name = argument; ArgumentType = argumentType }; Body = bodyBinding; ResultType = inferType bodyBinding }
            { Environment = bindingState.Environment.Add(name, FunctionBinding defun); Statements = bindingState.Statements @ [ Defun defun ] }
        | _, None -> 
            { bindingState with Statements = bindingState.Statements @ [ Ignore (ErrorBinding (sprintf "Could not infer type of argument %s." argument)) ] }
    | DefunExpr (name, _, _) -> 
        { bindingState with Statements = bindingState.Statements @ [ Ignore (ErrorBinding (sprintf "Function '%s' is already defined." name)) ] }
    | _ -> 
        let statement', _ = toBinding bindingState.Environment expression
        { bindingState with Statements = bindingState.Statements @ [ Ignore statement' ]}

let private builtins: Map<string, Binding> = 
    [
        ("inc", FunctionBinding { 
            Name = "inc"; 
            Argument = { Name = "value"; ArgumentType = IntType }    
            Body = IntBinding 0; 
            ResultType = IntType 
        })
    ] |> Map.ofList

let fromExpressions (expressions : Expression list) : Statement list = 
    let initialState = { Environment = builtins; Statements = List.empty }
    let statements = List.fold fromExpression initialState expressions
    statements.Statements

let private findError = function
| Ignore (ErrorBinding error) -> Some (error)
| _ -> None

let findAllErrors statements = 
    match statements |> List.choose findError with
    | [] -> succeed statements
    | errors -> fail (errors |> String.concat System.Environment.NewLine)
