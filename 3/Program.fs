open System

let parseLine (line: String) : List<bool> =
    line.ToCharArray()
    |> Seq.map Char.ToString
    |> Seq.map Int32.Parse
    |> Seq.map ((<>) 0)
    |> Seq.toList


let rows = 
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)
    |> Seq.map parseLine
    |> Seq.toList

let columns =
    rows
    |> List.transpose

let isOneMostCommon(s: seq<bool>): bool =
    let s = Seq.toList s
    let len = s.Length
    let count1 =
        s
        |> Seq.filter id
        |> Seq.length
    count1 > len / 2

let toBinary (s: seq<bool>) =
    let s =
        s
        |> Seq.map (fun c -> if c then "1" else "0")
    let str = String.Join("", s)
    Convert.ToInt32(str, 2)

let gamma =
    columns
    |> List.map isOneMostCommon

let epsilon =
    gamma
    |> Seq.map not

let gamma2 =
    gamma |> toBinary

let epsilon2 =
    epsilon |> toBinary

printfn $"Part 1: gamma={gamma2} epsilon={epsilon2}, power={gamma2 * epsilon2}"

let rec findOxygen (rows: List<List<bool>>, column: int): int =
    printfn "%A" rows
    printfn $"{column}"
    match rows.Length with
    | 1 -> toBinary rows.[0]
    | 0 -> failwith "no scrubs"
    | _ ->
        let columns = List.transpose rows
        let bitsInColumn = columns.[column]
        let oneCount =
            bitsInColumn
            |> Seq.filter id
            |> Seq.length
        let columnHeight = List.length bitsInColumn
        let zeroCount = columnHeight - oneCount
        let booleanToKeep = oneCount >= zeroCount
        printfn $"size: {columnHeight}, oneCount: {oneCount}, keep: {booleanToKeep}"
        let rowFilter (row: List<bool>): bool =
            row.[column] = booleanToKeep

        let nextColumn = column + 1
        let filteredRows = (List.filter rowFilter rows)
        findOxygen(filteredRows, nextColumn)

// A lot of copy-paste below. Oh well, can't be bothered cleaning it up :-)
let rec findScrubber (rows: List<List<bool>>, column: int): int =
    printfn "%A" rows
    printfn $"{column}"
    match rows.Length with
    | 1 -> toBinary rows.[0]
    | 0 -> failwith "no scrubs"
    | _ ->
        let columns = List.transpose rows
        let bitsInColumn = columns.[column]
        let oneCount =
            bitsInColumn
            |> Seq.filter id
            |> Seq.length
        let columnHeight = List.length bitsInColumn
        let zeroCount = columnHeight - oneCount
        let booleanToKeep = oneCount < zeroCount
        printfn $"size: {columnHeight}, oneCount: {oneCount}, keep: {booleanToKeep}"
        let rowFilter (row: List<bool>): bool =
            row.[column] = booleanToKeep

        let nextColumn = column + 1
        let filteredRows = (List.filter rowFilter rows)
        findScrubber(filteredRows, nextColumn)

let scrubber =
    findScrubber(rows, 0)

let oxygen =
    findOxygen(rows, 0)
  

printfn $"Part 2: oxygen={oxygen} scrubber={scrubber} life_support={oxygen * scrubber}"