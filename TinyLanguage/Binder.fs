module Binder

open Binding
open Context
open Railway
open Syntax

let private argumentExpressionToArgument (expr: ArgumentExpression) =
    {
        ArgumentName = expr.ArgumentName
        ArgumentType = 
            match expr.TypeName with
            | "int"    -> IntType
            | "bool"   -> BoolType
            | "string" -> StringType
            | "unit"   -> UnitType
            | wrong    -> ErrorType (sprintf "Expected argument type; found '%s'." wrong)
    }


let rec inferType (binding: Binding) : Type =
    match binding with
    | BoolBinding   _ -> BoolType
    | IntBinding    _ -> IntType
    | IncBinding    _ -> IntType
    | StringBinding _ -> StringType
    | NilBinding      -> UnitType
    | VariableBinding (_, variableType) -> variableType
    | FunctionBinding (Inc _) ->
        FunctionType(IntType, IntType)
    | FunctionBinding ( UserFunction (Argument = argument; ResultType = resultType )) ->
        FunctionType (argument.ArgumentType, resultType)
    | InvokeBinding { Argument = _; Function = Inc } ->
        FunctionType (IntType, IntType)
    | InvokeBinding { Argument = argument; Function = UserFunction (_, _, resultType) } ->
        FunctionType (argument |> inferType, resultType)
    | DefBinding { Body = body } -> body |> inferType
    | ErrorBinding message -> ErrorType message

let private argumentTypeError (invokedBinding: Binding) (func: Function) =
    let definedArgumentType = 
        match func with
        | Inc -> IntType
        | UserFunction (argument, _, _) -> argument.ArgumentType
    match invokedBinding |> inferType with 
    | bindingType                  when bindingType = definedArgumentType -> None
    | FunctionType (_, resultType) when resultType  = definedArgumentType -> None
    | _ -> 
        Some (sprintf "Expected %s argument, but found %s." (definedArgumentType |> prettyPrintType) (invokedBinding |> prettyPrintBinding))


let rec private toBinding (environment: Context) (expression : Expression) : Binding =
    match expression with
    | NilExpr                     -> failwith "() should never happen here"
    | IdentifierExpr name               -> 
        match environment.TryFind name with
        | Some binding -> binding
        | _            -> ErrorBinding (sprintf "Unrecognized identifier '%s'." name)
    | IntExpr n                         -> IntBinding n
    | StringExpr str                    -> StringBinding str
    | DefunExpr (_name, argumentExpression, body) -> 
        let argument = argumentExpression |> argumentExpressionToArgument
        let bodyEnvironment =
            match argument.ArgumentType with
            | UnitType -> environment
            | _ -> Context.extend environment (argument.ArgumentName, VariableBinding(variableName = argument.ArgumentName, variableType = argument.ArgumentType))
        let bodyBinding = toBinding bodyEnvironment body
        FunctionBinding (UserFunction (argument, bodyBinding, bodyBinding |> inferType))
    | InvokeExpr (name, argument)       -> 
        match environment.TryFind name with
        | Some (FunctionBinding func) -> 
            let argumentBinding = toBinding environment argument
            match argumentTypeError argumentBinding func with
            | None ->
                InvokeBinding { 
                    FunctionName = name
                    Function = func
                    Argument = argumentBinding
                }
            | Some argumentTypeErrorMessage ->
                ErrorBinding argumentTypeErrorMessage
        | Some bindingType -> ErrorBinding (sprintf "Expected function; found %A" bindingType)
        | None -> ErrorBinding (sprintf "Undefined function '%s'." name)
    | ErrorExpr error -> ErrorBinding error
 
let rec private expressionsToBinding (environment: Context) (expressions : Expression list) : Binding = 
    match expressions with
    | expression :: rest -> 
        match expression with
        | DefunExpr (name, _argument, _body) when (not (environment.ContainsKey name)) ->  
            let defunBinding = expression |> toBinding environment
            match defunBinding with
            | FunctionBinding functionBinding -> 
                let environment' = environment.Add(name, defunBinding)
                let body = 
                    match name with 
                    | "main" ->
                        InvokeBinding { FunctionName = "main"; Function = functionBinding; Argument = NilBinding }
                    | _ -> 
                        expressionsToBinding environment' rest
                DefBinding { 
                    VariableName =    name
                    VariableBinding = defunBinding
                    Body =            body }
            | notFunction -> ErrorBinding (sprintf "Sorry, we don't support %A bindings just yet." notFunction)
        | DefunExpr (name, _, _) -> 
            ErrorBinding (sprintf "Function '%s' is already defined." name)
        | _ -> 
            match rest with 
            | []  -> toBinding environment expression
            | _   -> ErrorBinding "Unexpected extra statements."
    | [] -> ErrorBinding "Expected statement here."


let bind (expressions : Expression list) : Binding = 
    expressionsToBinding Context.intrinsics expressions

let rec bindingExists (predicate: Binding -> bool) (binding: Binding) = 
    if (predicate(binding))
    then true
    else
    match binding with 
        | FunctionBinding (UserFunction(_, body, _)) -> 
            bindingExists predicate body  
        | InvokeBinding  { Argument = argumentBinding } ->
            bindingExists predicate argumentBinding
        | DefBinding { VariableBinding = variableBinding; Body = bodyBinding } ->
            bindingExists predicate variableBinding || bindingExists predicate bodyBinding
        | BoolBinding _ 
        | FunctionBinding Inc
        | IncBinding _
        | IntBinding  _ 
        | InvokeBinding _
        | NilBinding
        | StringBinding _
        | VariableBinding _
        | ErrorBinding _    -> 
            false

let rec private findErrors = function
| ErrorBinding error -> 
    [ error ]
| FunctionBinding (UserFunction(_, body, _)) -> 
    findErrors body  
| InvokeBinding  { Argument = argumentBinding } ->
    findErrors argumentBinding
| DefBinding { VariableBinding = variableBinding; Body = bodyBinding } ->
    findErrors variableBinding @ findErrors bodyBinding
| BoolBinding _ 
| FunctionBinding Inc
| IncBinding _
| IntBinding  _ 
| InvokeBinding _
| NilBinding
| StringBinding _
| VariableBinding _ -> 
    []

let failIfAnyErrors (statements : Binding) = 
    match statements |> findErrors with
    | [] -> succeed statements
    | errors -> fail (errors |> String.concat System.Environment.NewLine)
