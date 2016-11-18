﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Cell =
    | Fixed of int
    | Open of Set<int>

type Board = Map<int * int, Cell>

let related (x, y) =
    seq {
        for i  in 0..8 ->(i,y)
        for i  in 0..8 ->(x,i)
        for i  in x/3 .. x/3 + 2 do
            for j in y/3 .. y/3 + 2 
             ->(i,j)} |> Set.ofSeq |> Set.remove (x,y)
   
   
let lines = System.IO.File.ReadAllLines("C:/Users/valaki/Desktop/input.txt")

let input = Array2D.init 9 9 (fun i j ->
                     match (int)lines.[i].[j .. j] with
                     | x when x>0 -> Some x 
                     | _ -> None ) 

let get (x, y) (arr: _[,]) = arr.[x, y]

let pos = 3, 4

let e = board |> get pos
related(2,2) |> List.ofSeq
 |> List.length



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
