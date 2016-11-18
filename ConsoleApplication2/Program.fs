// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Cell =
    | Fixed of int
    | Open of Set<int>

type Board = Cell[,]

let lines = System.IO.File.ReadAllLines("C:/Users/valaki/Desktop/input.txt")

let board = Array2D.init 9 9 (fun i j ->
                     match (int)lines.[i].[j] with
                     |x when x>0 -> x 
                     |0 -> ) 








[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
