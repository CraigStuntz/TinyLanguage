module Binder

open Binding
open Context
open Railway
open Syntax

let private toArgumentBinding = function
| Some (argument: ArgumentExpression) -> 
    Some {
        ArgumentName = argument.ArgumentName
        ArgumentType = 
            match argument.TypeName with
            | "int"    -> IntType
            | "bool"   -> BoolType
            | "string" -> StringType
            | wrong    -> ErrorType (sprintf "Expected argument type; found '%s'." wrong)
    }
| None -> None

let rec inferType (binding: Binding) : Type =
    match binding with
    | BoolBinding   _ -> BoolType
    | IntBinding    _ -> IntType
    | IncBinding    _ -> IntType
    | StringBinding _ -> StringType
    | VariableBinding (variableType = variableType) -> variableType
    | FunctionBinding (Inc _) ->
        FunctionType(Some IntType, IntType)
    | FunctionBinding ( UserFunction (Argument = argument; ResultType = resultType )) ->
        match argument with
        | Some argumentBinding -> FunctionType (Some argumentBinding.ArgumentType, resultType)
        | None                 -> FunctionType (None, resultType)
    | InvokeBinding { Argument = _; Function = Inc } ->
        FunctionType (Some IntType, IntType)
    | InvokeBinding { Argument = argument; Function = UserFunction (_, _, resultType) } ->
        let argType = 
            match argument with
            | Some arg -> Some(arg |> inferType)
            | None     -> None
        FunctionType (argType, resultType)
    | DefBinding { Body = body } -> body |> inferType
    | ErrorBinding message -> ErrorType message

let private argumentTypeError (invokedBinding: Binding option) (func: Function) =
    let definedArgumentType = 
        match func with
        | Inc -> Some IntType
        | UserFunction (Some argument, _, _) -> Some argument.ArgumentType
        | UserFunction _ -> None
    match invokedBinding, definedArgumentType with
    | None, None -> 
        None
    | Some invoked, None -> 
        Some (sprintf "Expected no arguments, but found %s." (invoked |> prettyPrintBinding))
    | None, Some defined -> 
        Some (sprintf "Expected %s argument, but no arguments given." (defined |> prettyPrintType))
    | Some invoked, Some defined ->
        match invoked |> inferType with 
        | bindingType                  when bindingType = defined -> None
        | FunctionType (_, resultType) when resultType  = defined -> None
        | _ -> 
            Some (sprintf "Expected %s argument, but found %s." (defined |> prettyPrintType) (invoked |> prettyPrintBinding))


let rec private toInvokedArgumentBinding (environment: Context) (expression : Expression option) : Binding option =
    match expression with 
    | None -> 
        None 
    | Some invokedArgument -> 
        Some(toBinding environment invokedArgument)

and private toBinding (environment: Context) (expression : Expression) : Binding =
    match expression with
    | EmptyListExpr                     -> failwith "() should never happen here"
    | IdentifierExpr name               -> 
        match environment.TryFind name with
        | Some binding -> binding
        | _            -> ErrorBinding (sprintf "Unrecognized identifier '%s'." name)
    | IntExpr n                         -> IntBinding n
    | StringExpr str                    -> StringBinding str
    | DefunExpr (name = _name; argument = argument; body = body) -> 
        let argumentBinding = argument |> toArgumentBinding
        let bodyEnvironment =
            match argumentBinding with
            | Some binding -> Context.extend environment (binding.ArgumentName, binding.ArgumentType)
            | None -> environment
        let bodyBinding = toBinding bodyEnvironment body
        FunctionBinding (UserFunction (argumentBinding, bodyBinding, bodyBinding |> inferType))
    | InvokeExpr (name, argument)       -> 
        match environment.TryFind name with
        | Some (FunctionBinding func) -> 
            let argumentBinding = toInvokedArgumentBinding environment argument
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
                        InvokeBinding { FunctionName = "main"; Function = functionBinding; Argument = None }
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
        | InvokeBinding  { Argument = Some argumentBinding } ->
            bindingExists predicate argumentBinding
        | DefBinding { VariableBinding = variableBinding; Body = bodyBinding } ->
            bindingExists predicate variableBinding || bindingExists predicate bodyBinding
        | BoolBinding _ 
        | FunctionBinding Inc
        | IncBinding _
        | IntBinding  _ 
        | InvokeBinding _
        | StringBinding _
        | VariableBinding _
        | ErrorBinding _    -> 
            false

let rec private findErrors = function
| ErrorBinding error -> 
    [ error ]
| FunctionBinding (UserFunction(_, body, _)) -> 
    findErrors body  
| InvokeBinding  { Argument = Some argumentBinding } ->
    findErrors argumentBinding
| DefBinding { VariableBinding = variableBinding; Body = bodyBinding } ->
    findErrors variableBinding @ findErrors bodyBinding
| BoolBinding _ 
| FunctionBinding Inc
| IncBinding _
| IntBinding  _ 
| InvokeBinding _
| StringBinding _
| VariableBinding _ -> 
    []

let failIfAnyErrors (statements : Binding) = 
    match statements |> findErrors with
    | [] -> succeed statements
    | errors -> fail (errors |> String.concat System.Environment.NewLine)
