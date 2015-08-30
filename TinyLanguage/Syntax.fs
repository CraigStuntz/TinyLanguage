module Syntax

type Operation = | Plus | Minus | Times

type Function =
    | Builtin of Operation

type Expression = 
    | IntExpr     of int
    | DefunExpr   of string   * Expression list
    | InvokeExpr  of Function * Expression list
    | ErrorExpr   of string