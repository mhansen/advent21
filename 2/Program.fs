open System

type Position = int * int

type Command = 
  | Up of int
  | Down of int
  | Forward of int

let parseLine (line : String) : Option<Command> =
    let words = line.Trim().Split " "
    let command = words.[0]
    let amount = Int32.Parse(words.[1])
    match command with
    | "forward" ->  Some(Forward amount)
    | "up" ->  Some(Up amount)
    | "down" -> Some(Down amount)
    | _ -> None

let positionDiff (command: Command): Position =
    match command with
    | Up(amount) -> -amount, 0
    | Down(amount) -> amount, 0
    | Forward(amount) -> 0, amount

let reducePositions (p1: Position) (p2: Position) : Position =
    let x1, y1 = p1
    let x2, y2 = p2
    (x1 + x2, y1 + y2)

let commands =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)
    |> Seq.map parseLine
    |> Seq.choose id
    |> Seq.toList

let x, y =
    commands
   |> Seq.map positionDiff
   |> Seq.reduce reducePositions

printfn "Part 1: %Ax%A=%A" x y (x * y)

type PositionAndAim = {
    Depth: int
    Horizontal: int
    Aim: int
}

let positionAndAimDiff (command: Command): PositionAndAim =
    match command with
    | Up(amount)   -> {Depth=0; Horizontal=0; Aim=(-amount);}
    | Down(amount) -> {Depth=0; Horizontal=0; Aim=amount;}
    | Forward(amount) -> {Depth=0; Horizontal=amount; Aim=0}

let reducePositionsAndAims (p: PositionAndAim) (p2: PositionAndAim): PositionAndAim =
    {  
        Depth=p.Depth + p2.Horizontal * p.Aim;
        Aim=p.Aim + p2.Aim;
        Horizontal=p.Horizontal + p2.Horizontal;
    }

let p2 =
    commands
    |> Seq.map positionAndAimDiff
    |> Seq.reduce reducePositionsAndAims

printfn "Part 2: %Ax%A=%A" p2.Depth p2.Horizontal (p2.Depth * p2.Horizontal)
 