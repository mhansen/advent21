open System

let numbers =
    Console.ReadLine().Trim().Split(",")
    |> Seq.map Int32.Parse
    |> Seq.toList

let rows =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)
    |> Seq.toList


let parseBoardRow (row: string) : list<int> =
    row.Trim().Split(" ")
    |> Array.toSeq
    |> Seq.filter ((<>) "")
    |> Seq.map Int32.Parse
    |> Seq.toList

type Board = list<list<int>>

let parseBoard (board: seq<string>) : Board  =
    board
    |> Seq.map parseBoardRow
    |> Seq.toList

let boards =
    rows
    |> Seq.chunkBySize 6
    |> Seq.map (fun x -> x[1..])
    |> Seq.map parseBoard
    |> Seq.toList

type Marking = list<list<bool>>

let win_diagonal_1 (board: Marking): bool =
    board.[0].[0] && board.[1].[1] && board.[2].[2] && board.[3].[3] && board.[4].[4]

let win_diagonal_2 (board: Marking): bool =
    board.[4].[0] && board.[3].[1] && board.[2].[2] && board.[1].[3] && board.[0].[4]

let win_horizontal (board: Marking): bool =
    board
    |> Seq.exists (fun x -> Seq.forall id x)

let win (board: Marking): bool =
    win_horizontal board
    || win_horizontal (List.transpose board)

type BoardAndMarking = {
    Board: Board
    Marking: Marking
}

let win_bm (bm: BoardAndMarking): bool =
    win bm.Marking

type BingoState = list<BoardAndMarking>

let or_line (b1: list<bool>) (b2: list<bool>): list<bool> =
    List.map2 (fun x y -> x || y) b1 b2

let or_marking (b1: Marking) (b2: Marking): Marking =
    List.map2 or_line b1 b2

let mark_line(number: int) (l: list<int>): list<bool> =
    l
    |> List.map ((=) number)

let mark_board (number: int) (b: Board): Marking =
    b
    |> List.map (mark_line number)

let call_number_for_board (number: int) (b: BoardAndMarking): BoardAndMarking =
    let marking = (mark_board number b.Board)
    {
        Board=b.Board
        Marking=or_marking b.Marking marking
    }

let call_number(number: int) (state: BingoState): BingoState =
    state
    |> List.map (call_number_for_board number)

type Score = {
    Board: BoardAndMarking
    UnmarkedNumbers: Board
    UnmarkedSum: int
    LastNumberCalled: int
    Score: int
}

let unmarked_numbers_row (row: list<int>) (marking: list<bool>): list<int> =
    List.map2 (fun x y -> if not y then x else 0) row marking

let unmarked_numbers (bm: BoardAndMarking): Board =
    List.map2 unmarked_numbers_row bm.Board bm.Marking

let rec play (state: BingoState) (nextNumbers: list<int>): Score =
    let number = nextNumbers[0]
    let newState = call_number number state

    let winning_boards =
        newState
        |> List.filter win_bm
    if winning_boards.Length > 0 then
        let winning_board = winning_boards[0]
        let unmarked = unmarked_numbers winning_board
        let unmarked_sum = List.sum (List.map List.sum unmarked)
        {
            LastNumberCalled=number
            UnmarkedNumbers=unmarked
            UnmarkedSum=unmarked_sum
            Score=unmarked_sum * number
            Board=winning_board
        }
    else play newState (nextNumbers[1..])

printfn "numbers=%A" numbers
printfn "rows=%A" rows
// printfn "score=%A" (score boards)
let initial_boards_and_markings: list<BoardAndMarking> =
    boards
    |> List.map (fun b -> {
        Board=b
        Marking=[
            [false; false; false; false; false]
            [false; false; false; false; false]
            [false; false; false; false; false]
            [false; false; false; false; false]
            [false; false; false; false; false]
        ]
    })

let print_marking (m: Marking) =
    printfn "Marking:"
    m
    |> List.map (printfn "%A")
    printfn "win: %A" (win m)

let print_board (b: Board) =
    printfn "Board:"
    b
    |> List.map (printfn "%A")

let print_boards (b: list<Board>) =
    b
    |> List.map print_board

let print_board_and_marking (bm: BoardAndMarking) =
    print_board bm.Board
    print_marking bm.Marking

let print_boards_and_markings (s: BingoState) =
    s
    |> List.map print_board_and_marking

// print_boards_and_markings (call_number 22(call_number 13(call_number 17(call_number 11 (call_number 0 (initial_boards_and_markings))))))

let winning_score = (play (initial_boards_and_markings) (numbers))
printfn "%A" winning_score
print_board_and_marking winning_score.Board