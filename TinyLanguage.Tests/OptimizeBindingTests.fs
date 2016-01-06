module OptimizeBindingTests

open NUnit.Framework
open FsUnit
open BindingTree
open OptimizeBinding

type OptimizeBindingTests() =
    let stubUserFunction = Inc // ignored; just needs to compile

    [<Test>]
    member this.``should optimize (inc -1)``() = 
        let binding = IncBinding(IntBinding -1)

        let actual = binding |> optimize

        actual |> should equal (IntBinding 0)


    
    [<Test>]
    member this.``should optimize nested (inc -1)``() = 
        let binding = InvokeBinding {
            FunctionName = "foo"
            Function = stubUserFunction
            Argument = Some (IncBinding (IntBinding -1))
        }
        let expected = InvokeBinding {
            FunctionName = "foo"
            Function = stubUserFunction
            Argument = Some (IntBinding 0)
        }
        let actual = binding |> optimize

        actual |> should equal expected
