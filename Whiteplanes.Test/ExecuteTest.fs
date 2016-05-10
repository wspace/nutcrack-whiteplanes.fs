namespace Whiteplanes.Fshape.Test

open System
open System.IO
open System.Text
open Microsoft.VisualStudio.TestTools.UnitTesting
open Whiteplanes.Fshape

[<TestClass>]
type ExecuteTest() = 

    [<TestMethod>]
    [<DeploymentItem("HelloWorld.ws")>]
    member this.testHelloWorld() =
        let console = new StringWriter()
        Console.SetOut(console)

        use fs = new FileStream("HelloWorld.ws", FileMode.Open, FileAccess.Read, FileShare.None)
        let interpreter = Whiteplanes(fs)
        interpreter.Run(
            { 
                Stack = [||];
                Heap = Map.ofList [];
                Labels = Map.ofList [];
                Callstack =[||];
                ProgramCounter = 0; 
                Cinput  = (fun () -> 'H')
                Iinput  = (fun () -> 72)
                Coutput = (fun character -> Console.Write(character)); 
                Ioutput = (fun integer   -> Console.Write(integer));
            }
        )
        Assert.AreEqual("Hello World", console.GetStringBuilder().ToString().Trim())

    [<TestMethod>]
    [<DeploymentItem("HeapControl.ws")>]
    member this.testHeapControl() = 
        let console = new StringWriter()
        Console.SetOut(console)

        use fs = new FileStream("HeapControl.ws", FileMode.Open, FileAccess.Read, FileShare.None)
        let interpreter = Whiteplanes(fs)
        interpreter.Run(
            { 
                Stack  = [||];
                Heap   = Map.ofList [];
                Labels = Map.ofList [];
                ProgramCounter = 0; 
                Callstack =[||];
                Cinput  = (fun () -> 'H')
                Iinput  = (fun () -> 72)
                Coutput = (fun character -> Console.Write(character)); 
                Ioutput = (fun integer   -> Console.Write(integer));
            }
        )
        Assert.AreEqual("Hello World", console.GetStringBuilder().ToString().Trim())

    [<TestMethod>]
    [<DeploymentItem("FlowControl.ws")>]
    member this.testFlowControl() = 
        let console = new StringWriter()
        Console.SetOut(console)

        use fs = new FileStream("FlowControl.ws", FileMode.Open, FileAccess.Read, FileShare.None)
        let interpreter = Whiteplanes(fs)
        interpreter.Run(
            { 
                Stack  = [||];
                Heap   = Map.ofList [];
                Labels = Map.ofList [];
                ProgramCounter = 0; 
                Callstack =[||];
                Cinput  = (fun () -> 'H')
                Iinput  = (fun () -> 72)
                Coutput = (fun character -> Console.Write(character)); 
                Ioutput = (fun integer   -> Console.Write(integer));
            }
        )
        Assert.AreEqual("52", console.GetStringBuilder().ToString().Trim())

    [<TestMethod>]
    [<DeploymentItem("Count.ws")>]
    member this.testCount() =
        let console = new StringWriter()
        Console.SetOut(console)

        use fs = new FileStream("Count.ws", FileMode.Open, FileAccess.Read, FileShare.None)
        let interpreter = Whiteplanes(fs)
        interpreter.Run(
            { 
                Stack  = [||];
                Heap   = Map.ofList [];
                Labels = Map.ofList [];
                ProgramCounter = 0; 
                Callstack =[||];
                Cinput  = (fun () -> 'H')
                Iinput  = (fun () -> 72)
                Coutput = (fun character -> Console.Write(character)); 
                Ioutput = (fun integer   -> Console.Write(integer));
            }
        )
        Assert.AreEqual("1\n2\n3\n4\n5\n6\n7\n8\n9\n10", console.GetStringBuilder().ToString().Trim())

    [<TestMethod>]
    [<DeploymentItem("Input.ws")>]
    member tehis.testInput() =
        let console = new StringWriter()
        Console.SetOut(console)

        use fs = new FileStream("Input.ws", FileMode.Open, FileAccess.Read, FileShare.None)
        let interpreter = Whiteplanes(fs)
        interpreter.Run(
            { 
                Stack  = [||];
                Heap   = Map.ofList [];
                Labels = Map.ofList [];
                ProgramCounter = 0; 
                Callstack =[||];
                Cinput  = (fun () -> 'H')
                Iinput  = (fun () -> 72)
                Coutput = (fun character -> Console.Write(character)); 
                Ioutput = (fun integer   -> Console.Write(integer));
            }
        )
        Assert.AreEqual("H72", console.GetStringBuilder().ToString().Trim())
        