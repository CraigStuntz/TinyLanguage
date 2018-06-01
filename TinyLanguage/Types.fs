module Types

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
