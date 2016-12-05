// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
//todo: get input and output filenames as command line arguments
//todo: write output to file

type Cell =
    | Fixed of int
    | Open of Set<int>
    override self.ToString() =
        match self with
        | Fixed f -> sprintf "%i" f
        | Open s when s.IsEmpty -> sprintf "x"
        | Open _ -> sprintf("?") 
        

type Board = Map<int * int, Cell>

let boardToArray (board:Board option)=
  match board with
    |Some board-> 
        Array2D.init 9 9 (fun i j ->
            match board.[i,j] with
            | Fixed f -> f
            |_->0)
     |_->failwith "No Board!"
let printBoard (board:Board option) =
    match board with
    |Some b-> 
        for i = 0 to 8 do
            for j = 0 to 8 do
                printf "%O" b.[i,j]
            printfn ""
    |_->printfn"invalid input!"
        
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

   
type Result<'a> =
    | Ok of 'a
    | Error of string

__SOURCE_DIRECTORY__

open System.IO 
System.Environment.CurrentDirectory


//let readInput (dirname:string) (filename:string): string[] option = 
//    
//        try
//            Some (File.ReadAllLines(Path.Combine (dirname,filename)))
//        with
//        | :? FileNotFoundException as ex -> None
//        | :? DirectoryNotFoundException as ex -> None
//   
let lines  =
    try
      Some (File.ReadAllLines(Path.Combine (__SOURCE_DIRECTORY__,"input.txt")))
    with
    | :? FileNotFoundException as ex -> None
    | :? DirectoryNotFoundException as ex -> None

            
let input (lines:string[])= 
    try
        Array2D.init 9 9 (fun i j ->
            match (int)lines.[i].[j .. j] with
            | x when x>0 -> Some x 
            | _ -> None )
        |> Some 
    with _ -> None
 
let input2 (lines:string[] option)= 
    match lines with   
    | Some lines -> 
        seq {
             for i  in 0..8  do  
                 for j in 0..8 do
                   if (int)lines.[i].[j .. j]>0  then yield((i,j),(int)lines.[i].[j .. j])
          } |>Seq.toList
    |_ ->failwith "No input file found!"

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

let startingBoard = input2 lines |> List.fold fixCell emptyBoard 
        
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
    

//let nextBoard = nextGuess startingBoard 

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

   
    
 // solve emptyBoard |>Option.iter printBoard  

//let e = board |> get pos
//related(2,2) |> List.ofSeq
// |> List.length
let testPart (part:int[]) =
   part |> Set.ofArray |> Set.count  |> (fun x->
            match x with
            |9 -> true
            |_-> false  )


let testPart2 (part:int[,])=
    seq {
        for i = 0 to 2 do
            for j = 0 to 2 do
                yield part.[i,j]
        } |> Set.ofSeq |> Set.count |> (fun x->
            match x with
            |9 -> true
            |_-> false  )
        
let testBoard (board:int[,]):bool=
  let mutable result= true
  for i=0 to 8 do
   if not (board.[i,0..8]|>testPart) then result<-false  
  for j=0 to 8 do
       if not (board.[0..8,j]|>testPart) then  result<-false 
  if not (board.[0..2,0..2]|>testPart2) then  result<-false  
  if not (board.[0..2,3..5]|>testPart2) then  result<-false 
  if not (board.[0..2,6..8]|>testPart2) then  result<-false 
  if not (board.[3..5,0..2]|>testPart2) then  result<-false                 
  if not (board.[3..5,3..5]|>testPart2) then  result<-false 
  if not (board.[3..5,6..8]|>testPart2) then  result<-false   
  if not (board.[6..8,0..2]|>testPart2) then  result<-false                  
  if not (board.[6..8,3..5]|>testPart2) then  result<-false 
  if not (board.[6..8,6..8]|>testPart2) then  result<-false 
  result

//let arr2 = Array2D.zeroCreate<int> 9 9

//arr2.[0 .. 2, 0 .. 2]


[<EntryPoint>]
let main argv =
   

    solve startingBoard  |> printBoard 
    solve startingBoard |> boardToArray |> testBoard
    solve emptyBoard |> printBoard  
    solve startingBoard 
    
    0 // return an integer exit code
