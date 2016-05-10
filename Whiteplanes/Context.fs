namespace Whiteplanes.Fshape

type Context = { 
    Stack : array<int>;
    Heap : Map<int,int>;
    Labels : Map<string, int>;
    Callstack : array<int>
    ProgramCounter : int; 
    Cinput : (unit -> char);
    Iinput : (unit -> int);
    Coutput : (string -> unit);
    Ioutput : (int -> unit); 
    }