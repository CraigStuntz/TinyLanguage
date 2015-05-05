module Il

open System
open System.Reflection
open System.Reflection.Emit

type Instruction = 
    | Add 
    | Call         of System.Reflection.MethodInfo
    | Callvirt     of System.Reflection.MethodInfo
    | DeclareLocal of System.Type
    | Div
    | Ldc_I4       of int
    | Ldc_I4_0
    | Ldc_I4_1
    | Ldc_I4_2
    | Ldc_I4_3
    | Ldc_I4_4
    | Ldc_I4_5
    | Ldc_I4_6
    | Ldc_I4_7
    | Ldc_I4_8
    | Ldloc        of int
    | Ldloc_0
    | Ldloc_1
    | Ldloc_2
    | Ldloc_3
    | Ldloc_S      of byte
    | Mul
    | Neg
    | Newobj       of System.Reflection.ConstructorInfo
    | Nop
    | Pop
    | Refanyval
    | Ret
    | Stloc        of int
    | Stloc_0
    | Stloc_1
    | Stloc_2
    | Stloc_3
    | Stloc_S      of byte
    | Sub

type Method = { Instructions: Instruction list } 
     
let private emit (ilg : Emit.ILGenerator) inst = 
    match inst with 
    | Add            -> ilg.Emit(OpCodes.Add)
    | Call mi        -> ilg.Emit(OpCodes.Call, mi)
    | Callvirt mi    -> ilg.Emit(OpCodes.Callvirt, mi)
    | DeclareLocal t -> ignore(ilg.DeclareLocal(t))
    | Div            -> ilg.Emit(OpCodes.Div)
    | Ldc_I4 n       -> ilg.Emit(OpCodes.Ldc_I4, n)
    | Ldc_I4_0       -> ilg.Emit(OpCodes.Ldc_I4_0)
    | Ldc_I4_1       -> ilg.Emit(OpCodes.Ldc_I4_1)
    | Ldc_I4_2       -> ilg.Emit(OpCodes.Ldc_I4_2)
    | Ldc_I4_3       -> ilg.Emit(OpCodes.Ldc_I4_3)
    | Ldc_I4_4       -> ilg.Emit(OpCodes.Ldc_I4_4)
    | Ldc_I4_5       -> ilg.Emit(OpCodes.Ldc_I4_5)
    | Ldc_I4_6       -> ilg.Emit(OpCodes.Ldc_I4_6)
    | Ldc_I4_7       -> ilg.Emit(OpCodes.Ldc_I4_7)
    | Ldc_I4_8       -> ilg.Emit(OpCodes.Ldc_I4_8)
    | Ldloc    i     -> ilg.Emit(OpCodes.Ldloc, i)
    | Ldloc_0        -> ilg.Emit(OpCodes.Ldloc_0)
    | Ldloc_1        -> ilg.Emit(OpCodes.Ldloc_1)
    | Ldloc_2        -> ilg.Emit(OpCodes.Ldloc_2)
    | Ldloc_3        -> ilg.Emit(OpCodes.Ldloc_3)
    | Ldloc_S  i     -> ilg.Emit(OpCodes.Ldloc_S, i)
    | Mul            -> ilg.Emit(OpCodes.Mul)
    | Neg            -> ilg.Emit(OpCodes.Neg)
    | Newobj   ci    -> ilg.Emit(OpCodes.Newobj, ci)
    | Nop            -> ilg.Emit(OpCodes.Nop)
    | Pop            -> ilg.Emit(OpCodes.Pop)
    | Refanyval      -> ilg.Emit(OpCodes.Refanyval)
    | Ret            -> ilg.Emit(OpCodes.Ret)
    | Stloc    i     -> ilg.Emit(OpCodes.Stloc, i)
    | Stloc_0        -> ilg.Emit(OpCodes.Stloc_0)
    | Stloc_1        -> ilg.Emit(OpCodes.Stloc_1)
    | Stloc_2        -> ilg.Emit(OpCodes.Stloc_2)
    | Stloc_3        -> ilg.Emit(OpCodes.Stloc_3)
    | Stloc_S  i     -> ilg.Emit(OpCodes.Stloc_S, i)
    | Sub            -> ilg.Emit(OpCodes.Sub)

let private compileEntryPoint (moduleContainingMethod : ModuleBuilder) (methodToCall: MethodBuilder) = 
    let tb = 
        let className = "Program"
        let ta = TypeAttributes.NotPublic ||| TypeAttributes.AutoLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.BeforeFieldInit
        moduleContainingMethod.DefineType(className, ta)
    let mb = 
        let ma = MethodAttributes.Public ||| MethodAttributes.Static 
        let methodName = "Main"
        tb.DefineMethod(methodName, ma)
    let ilg = mb.GetILGenerator() |> emit
    let ci = methodToCall.ReflectedType.GetConstructor([||])
    ilg (Call methodToCall)
    if methodToCall.ReturnType <> null then
        ilg (DeclareLocal methodToCall.ReturnType)
        ilg Stloc_0
        ilg (Ldloc_S 0uy)
        let writeln = typeof<System.Console>.GetMethod("WriteLine", [| typeof<System.Int32> |])
        ilg (Call writeln)
    ilg Ret
    tb.CreateType() |> ignore
    mb

let compileMethod(typeBuilder: TypeBuilder, methodName: string, instructions: seq<Instruction>) =
    let methodBuilder = 
        let methodAttributes = MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.HideBySig
        typeBuilder.DefineMethod(methodName, methodAttributes, typeof<int>, System.Type.EmptyTypes)
    let ilg = methodBuilder.GetILGenerator() |> emit
    Seq.iter ilg instructions 
    ilg Ret
    methodBuilder

let private compileModule(moduleName: string, methods: Map<string, Instruction list>) =
    let assemblyBuilder = 
        let assemblyName = AssemblyName(moduleName)
        AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(moduleName)
    let typeBuilder = 
        let className = "__CompiledMethods"
        let typeAttributes = TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| TypeAttributes.AutoLayout ||| TypeAttributes.AnsiClass ||| TypeAttributes.BeforeFieldInit
        moduleBuilder.DefineType(className, typeAttributes)
    let methodBuilders = 
        methods 
        |> Map.map (fun name instructions -> compileMethod(typeBuilder, name, instructions))
    let entryPointType = typeBuilder.CreateType()
    let entryPoint = compileEntryPoint moduleBuilder methodBuilders.["main"]
    assemblyBuilder.SetEntryPoint(entryPoint, PEFileKinds.ConsoleApplication)
    moduleBuilder.CreateGlobalFunctions()
    assemblyBuilder

let toAssemblyBuilder(methods: Map<string, Instruction list>) =
    let moduleName = "test.exe"
    compileModule(moduleName, methods)
