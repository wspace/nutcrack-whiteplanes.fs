namespace Whiteplanes.Fshape

open System.IO

type Whiteplanes(stream : Stream) = 
    let code = (new StreamReader(stream)).ReadToEnd()
    let commands = code.ToCharArray() 
                  |> Array.filter(Character.IsCharacter)
                  |> Command.Make


    member this.Run(context : Context) =
        let mutable cxt = commands
                          |> List.filter(fun (instruction, parameter) -> match instruction with | :? Register -> true | _ -> false)
                          |> List.fold (fun cxt (instruction, parameter) -> instruction.Execute(cxt, parameter)) context
        while cxt.ProgramCounter < commands.Length do
            let instruction, parameter = commands.[cxt.ProgramCounter]
            cxt <- match instruction with
                   | :? Register -> cxt
                   | _           -> instruction.Execute(cxt, parameter)
            cxt <- { cxt with ProgramCounter = cxt.ProgramCounter + 1 }
        ()