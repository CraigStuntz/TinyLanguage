module IlGenerator

open Il
open Parser
open Railway

let codegenDefuns (expressions : Expression list): Result<Map<string, Instruction list>, string> = 
    succeed Map.empty