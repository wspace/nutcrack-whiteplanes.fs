namespace Whiteplanes.Fshape

open System

exception SyntaxError  of string

type Character =
    static member Space = ' '
    static member Tab = '\t'
    static member Newline = '\n'
    static member IsCharacter chr = chr = Character.Space || chr = Character.Tab || chr = Character.Newline

type Instruction =
    abstract member Step : int
    abstract member IsNeedParameter : bool
    abstract member Pattern : (char option * char option * char option * char option)
    abstract member Execute : (Context * (string * int) -> Context)
    

type Push() = 
    let command(context : Context, parameter : (string * int)) = 
        let value, _ = parameter
        {context with Stack = Array.append context.Stack [|Convert.ToInt32(value, 2)|]}
    interface Instruction with
        member this.Step = 2
        member this.IsNeedParameter = true
        member this.Pattern = (Some(Character.Space), Some(Character.Space), None, None)
        member this.Execute = command

type Copy() =
    let command(context : Context, parameter : (string * int)) =
        let value, _ = parameter
        {context with Stack = Array.append context.Stack [|context.Stack.[Convert.ToInt32(value, 2)]|] }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = true
        member this.Pattern = (Some(Character.Space) , Some(Character.Tab), Some(Character.Space), None)
        member this.Execute = command

type Slide() =
    let command(context : Context, parameter : (string * int)) =
         let value, _ = parameter
         let index    = context.Stack.Length
         let slide    = Int32.Parse value + 2
         { context with Stack = Array.append context.Stack.[0..index - slide] [|context.Stack.[index - 1]|] }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = true
        member this.Pattern = (Some(Character.Space) , Some(Character.Tab), Some(Character.Newline), None)
        member this.Execute = command

type Duplicate() =
    let command(context : Context, parameter : (string * int)) = 
        let index = context.Stack.Length
        { context with Stack = Array.append context.Stack [|context.Stack.[index - 1]|] }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Space) , Some(Character.Newline), Some(Character.Space),  None)
        member this.Execute = command

type Swap() =
    let command(context : Context, parameter : (string * int)) = 
        let index = context.Stack.Length
        { context with Stack = Array.append context.Stack.[0..index - 3] [|context.Stack.[index - 1]; context.Stack.[index - 2]|] }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Space) , Some(Character.Newline), Some(Character.Tab), None)
        member this.Execute = command

type Discard() = 
    let command(context : Context, parameter : (string * int)) = 
        let index = context.Stack.Length
        { context with Stack = context.Stack.[0..index - 2] }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Space) , Some(Character.Newline), Some(Character.Newline), None)
        member this.Execute = command

type Add() = 
    let command(context : Context, parameter : (string * int)) = 
        let index = context.Stack.Length
        let value = context.Stack.[index - 1] + context.Stack.[index - 2]
        { context with Stack = Array.append context.Stack.[0..index - 3] [|value|] }
    interface Instruction with
        member this.Step = 4
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Space) , Some(Character.Space) , Some(Character.Space))
        member this.Execute = command

type Sub() = 
    let command(context : Context, parameter : (string * int)) = 
        let index = context.Stack.Length
        let value = context.Stack.[index - 1] - context.Stack.[index - 2]
        { context with Stack = Array.append context.Stack.[0..index - 3] [|value|] }
    interface Instruction with
        member this.Step = 4
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Space) , Some(Character.Space) , Some(Character.Tab))
        member this.Execute = command
type Mul() = 
    let command(context : Context, parameter : (string * int)) =
        let index = context.Stack.Length
        let value = context.Stack.[index - 1] * context.Stack.[index - 2]
        { context with Stack = Array.append context.Stack.[0..index - 3] [|value|] }
    interface Instruction with
        member this.Step = 4
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Space) , Some(Character.Space) , Some(Character.Newline))
        member this.Execute = command

type Div() =
    let command(context : Context, parameter : (string * int)) = 
        let index = context.Stack.Length
        let value = context.Stack.[index - 1] / context.Stack.[index - 2]
        { context with Stack = Array.append context.Stack.[0..index - 3] [|value|] }
    interface Instruction with
        member this.Step = 4
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Space) , Some(Character.Tab), Some(Character.Space)) 
        member this.Execute = command

type Mod() =
    let command(context : Context, parameter : (string * int)) = 
        let index = context.Stack.Length
        let value = context.Stack.[index - 1] % context.Stack.[index - 2]
        { context with Stack = Array.append context.Stack.[0..index - 3] [|value|] }
    interface Instruction with
        member this.Step = 4
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Space) , Some(Character.Tab), Some(Character.Tab)) 
        member this.Execute = command

type Store() =
    let command(context : Context, parameter : (string * int)) =
        let index   = context.Stack.Length
        let value   = context.Stack.[index - 1] 
        let address = context.Stack.[index - 2]
        { context with Stack = context.Stack.[0..index - 3]; Heap = Map.add address value context.Heap }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Tab), Some(Character.Space) , None) 
        member this.Execute = command

type Retrieve() =
    let command(context : Context, parameter : (string * int)) = 
        let index   = context.Stack.Length
        let address = context.Stack.[index - 1]
        let value   = Map.find address context.Heap
        { context with Stack = Array.append context.Stack.[0..index - 2] [|value|] }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Tab), Some(Character.Tab) , None) 
        member this.Execute = command

type Register() =
    let command(context : Context, parameter : (string * int)) = 
        let value, location = parameter
        { context with Labels = Map.add value location context.Labels}
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = true
        member this.Pattern = (Some(Character.Newline), Some(Character.Space) , Some(Character.Space) , None) 
        member this.Execute = command

type Call() =
    let command(context : Context, parameter : (string * int)) = 
        let value, location = parameter
        let index = Map.find value context.Labels
        { context with Callstack = Array.append context.Callstack [|location|]; ProgramCounter = index }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = true
        member this.Pattern = (Some(Character.Newline), Some(Character.Space) , Some(Character.Tab) , None) 
        member this.Execute = command

type Jump() =
    let command(context : Context, parameter : (string * int)) = 
        let value, location = parameter
        let index = Map.find value context.Labels
        { context with ProgramCounter = index }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = true
        member this.Pattern = (Some(Character.Newline), Some(Character.Space) , Some(Character.Newline) , None) 
        member this.Execute = command

type Equal() =
    let command(context : Context, parameter : (string * int)) = 
        let value, _ = parameter
        let index = context.Stack.Length
        let test  = context.Stack.[index - 1]
        { context with 
            Stack = context.Stack.[0..index - 2];
            ProgramCounter = if test = 0 then Map.find value context.Labels else context.ProgramCounter }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = true
        member this.Pattern = (Some(Character.Newline), Some(Character.Tab), Some(Character.Space) , None)
        member this.Execute = command

type Less() =
    let command(context : Context, parameter : (string * int)) =
        let value, _ = parameter
        let index = context.Stack.Length
        let test  = context.Stack.[index - 1]
        { context with
            Stack = context.Stack.[0..index - 2];
            ProgramCounter = if test < 0 then Map.find value context.Labels else context.ProgramCounter }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = true
        member this.Pattern = (Some(Character.Newline), Some(Character.Tab), Some(Character.Tab) , None)
        member this.Execute = command

type Return() =
    let command(context : Context, parameter : (string * int)) =
        let index = context.Callstack.Length 
        let value = context.Callstack.[index - 1];
        { context with Callstack = context.Callstack.[0..index - 2]; ProgramCounter = value}
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Newline), Some(Character.Tab), Some(Character.Newline) , None)
        member this.Execute = command

type End() =
    let command(context : Context, parameter : (string * int)) = 
        { context with ProgramCounter = Int32.MaxValue - 1 }
    interface Instruction with
        member this.Step = 3
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Newline), Some(Character.Newline), Some(Character.Newline) , None)
        member this.Execute = command

type Cout() =
    let command(context : Context, parameter : (string * int)) = 
        let index = context.Stack.Length
        let value = context.Stack.[index - 1]
        context.Coutput(Char.ConvertFromUtf32(value))
        { context with Stack = context.Stack.[0..index - 2] }
    interface Instruction with
        member this.Step = 4
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Newline), Some(Character.Space) , Some(Character.Space))
        member this.Execute = command

type Iout() =
    let command(context : Context, parameter : (string * int)) = 
        let index = context.Stack.Length
        let value = context.Stack.[index - 1]
        context.Ioutput(value)
        { context with Stack = context.Stack.[0..index - 2] }
    interface Instruction with
        member this.Step = 4
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Newline), Some(Character.Space) , Some(Character.Tab))
        member this.Execute = command

type Cin() =
    let command(context : Context, parameter : (string * int)) = 
        let index   = context.Stack.Length
        let address = context.Stack.[index - 1]
        let value   = Convert.ToInt32(context.Cinput())
        { context with Stack = context.Stack.[0..index - 2]; Heap = Map.add address value context.Heap  }
    interface Instruction with
        member this.Step = 4
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Newline), Some(Character.Tab), Some(Character.Space))
        member this.Execute = command

type Iin() =
    let command(context : Context, parameter : (string * int)) =
        let index   = context.Stack.Length
        let address = context.Stack.[index - 1]
        let value   = context.Iinput()
        { context with Stack = context.Stack.[0..index - 2]; Heap = Map.add address value context.Heap  }
    interface Instruction with
        member this.Step = 4
        member this.IsNeedParameter = false
        member this.Pattern = (Some(Character.Tab), Some(Character.Newline), Some(Character.Tab), Some(Character.Tab))
        member this.Execute = command

type Command() =
    static let instructions : list<Instruction> = [
        new Push(); 
        new Copy(); 
        new Slide(); 
        new Duplicate(); 
        new Swap(); 
        new Discard();
        new Add(); 
        new Sub(); 
        new Mul(); 
        new Div(); 
        new Mod();
        new Store(); 
        new Retrieve();
        new Register(); 
        new Call();
        new Jump();
        new Equal(); 
        new Less(); 
        new Return(); 
        new End();
        new Cout();
        new Iout();
        new Cin();
        new Iin();
    ]

    static member Make(source : array<char>) =
        let pause(cur : int) =
            let token = match (cur + 3) - source.Length with
                        | 2 -> (Some(source.[cur]), None, None, None)
                        | 1 -> (Some(source.[cur]), Some(source.[cur + 1]), None, None)
                        | 0 -> (Some(source.[cur]), Some(source.[cur + 1]), Some(source.[cur + 2]), None)
                        | _ -> (Some(source.[cur]), Some(source.[cur + 1]), Some(source.[cur + 2]), Some(source.[cur + 3]))
            let command = instructions 
                          |> List.find(fun instruction -> 
                             match instruction.Step with
                             | 2 -> let (tcn1, tcn2, _, _)    = token
                                    (tcn1, tcn2, None, None)  = instruction.Pattern
                             | 3 -> let (tcn1, tcn2, tcn3, _) = token
                                    (tcn1, tcn2, tcn3, None)  = instruction.Pattern
                             | 4 -> token = instruction.Pattern
                             | _ -> false)
            let index = cur + command.Step
            let parameter = match command.IsNeedParameter with
                            | true  ->
                                let separate = source.[index..] |> Array.findIndex(fun c -> c = Character.Newline)
                                source.[index..(index + separate)]
                                |> Array.map(fun c -> match c with ' ' -> "0" | '\t' -> "1" | _ -> "")
                                |> String.Concat
                            | false -> ""
            let step = if command.IsNeedParameter then index + parameter.Length + 1 else index
            (command, parameter, step)
        let mutable count, index = 0, 0
        [
            while count < source.Length do
                let command, parameter, step = pause(count)
                yield (command, (parameter, index))
                index <- index + 1
                count <- step
        ]