// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Cell =
    | Fixed of int
    | Open of Set<int>
    override self.ToString() =
        match self with
        | Fixed f -> sprintf "%i" f
        | Open s when s.IsEmpty -> sprintf "x"
        | Open _ -> sprintf("?") 
        

type Board = Map<int * int, Cell>
   
let printBoard (board:Board) =
    for i = 0 to 8 do
        for j = 0 to 8 do
            printf "%O" board.[i,j]
        printfn ""
        
type Guess =
    | NextBoard of Board 
    | NextGuess of ((int*int)*Set<int>)
    | Invalid
    | Complete of Board
let related (x, y) =
    let ib = x/3 * 3
    let jb= y/3 * 3
    seq {
        for i  in 0..8 ->(i,y)
        for i  in 0..8 ->(x,i)
        for i  in ib .. ib + 2 do
            for j in jb.. jb + 2 
             ->(i,j)
    } |> Set.ofSeq |> Set.remove (x,y)
//   csinálni kell egy seq-et, ami az inputban azokat az elemeket tartalmazza, amik nem üresek
//csinálni kell egy seq fold-ot, aminél az állapot a board, az elem ebből a seq-ből jön, és a függvény a fixcell
   
__SOURCE_DIRECTORY__

open System.IO 
System.Environment.CurrentDirectory

let lines =File.ReadAllLines(Path.Combine (__SOURCE_DIRECTORY__,"input.txt"))


            
let input = 
    Array2D.init 9 9 (fun i j ->
        match (int)lines.[i].[j .. j] with
        | x when x>0 -> Some x 
        | _ -> None ) 
let input2= 
    seq {
        for i  in 0..8  do  
            for j in 0..8 do
               if (int)lines.[i].[j .. j]>0  then yield((i,j),(int)lines.[i].[j .. j])
    } |>Seq.toList

let numbers = Set { 1 .. 9 }

let emptyCell = 
    Open(numbers)

let emptyBoard =
    Map [
        for i  in 0..8  do
            for j  in 0..8 ->((i,j),emptyCell)
    ]
 


let fixCell (board:Board) (pos, value) : Board =
    let boardWithFixed = board.Add(pos, Fixed value)
    related pos |> Seq.fold (fun board pos -> 
        match board.[pos] with 
        | Open s -> board.Add(pos, Open(s.Remove(value)))
        | _ -> board) boardWithFixed

let startingBoard = input2 |> List.fold fixCell emptyBoard 
        
let nextGuess (board:Board) :Guess =
    let opens=  board |>Map.toSeq |>Seq.choose  (fun cell ->
          match cell with
           |(pos,Open s)-> Some (pos,(s, Set.count s))
           |_-> None) |>Seq.toList
    if List.isEmpty opens then Complete(board) else
    let pos,(set, count) = opens |> List.minBy (snd >> snd)
    match count with
    |0 -> Invalid
    |1 -> fixCell board (pos,Set.minElement set) |> NextBoard
    |_ -> (pos,set) |> NextGuess
    

let nextBoard = nextGuess startingBoard

let rec solve (board:Board) = 
    let rec solveGuess (guess:Guess)=
        match guess with
        | NextBoard g -> nextGuess g |> solveGuess
        | NextGuess (pos,set) ->
            let s =
                set |> Seq.tryPick (fun v ->
                   fixCell board (pos,v) |> solve
                )
            match s with
            | Some g -> Complete(g)
            | None -> Invalid
        | Invalid -> Invalid 
        | Complete g -> Complete(g)
    match solveGuess (nextGuess board) with
    |Complete g -> Some g
    |_ -> None

        
solve startingBoard |>Option.iter printBoard     
    
solve emptyBoard |>Option.iter printBoard     
    


//let e = board |> get pos
//related(2,2) |> List.ofSeq
// |> List.length

let arr2 = Array2D.zeroCreate<int> 9 9

arr2.[0 .. 2, 0 .. 2]

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
