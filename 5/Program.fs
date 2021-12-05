open System

type Point = {
  X: int
  Y: int
}

type LineSegment = {
  A: Point
  B: Point
}

let parseLine (line: string) : LineSegment =
    let a = line.Split(" -> ")
    let start =
      a[0].Split(",")
      |> Array.map Int32.Parse
    let finish =
      a[1].Split(",")
      |> Array.map Int32.Parse
    {
      A= {
        X= start[0]
        Y= start[1]
      }
      B= {
        X=finish[0]
        Y=finish[1]
      }
    }

let is_horizontal (line: LineSegment) : bool = 
    line.A.X = line.B. X

let is_vertical (line: LineSegment) : bool = 
    line.A.Y = line.B.Y

let is_horizontal_or_vertical (line: LineSegment): bool =
    (is_horizontal line) || (is_vertical line)

let normalize_if_horizontal (line: LineSegment): LineSegment =
  if line.A.Y = line.B.Y && line.A.X > line.B.X then
    {A=line.B; B=line.A}
  else
    line

let normalize_if_vertical (line: LineSegment): LineSegment =
  if line.A.X = line.B.X && line.A.Y > line.B.Y then
    {A=line.B; B=line.A}
  else
    line

let normalize_if_diagonal (line: LineSegment): LineSegment =
  if line.A.X <> line.B.X && line.A.Y <> line.B.Y && line.A.X > line.B.X then
    {A=line.B; B=line.A}
  else
    line

let points_along_line (line: LineSegment) : seq<Point> =
    // Vertical
    if line.A.X = line.B.X then
      seq { for y in line.A.Y .. line.B.Y -> {X=line.A.X; Y=y} }
    // Horizontal
    elif line.A.Y = line.B.Y then
      seq { for x in line.A.X .. line.B.X -> {Y=line.A.Y; X=x} }
    // Diagonal 1
    elif line.A.Y > line.B.Y then
      seq { for x in line.A.X .. line.B.X -> {Y=line.A.Y + (line.A.X-x); X=x} }
    // Diagonal 2
    else
      seq { for x in line.A.X .. line.B.X -> {Y=line.A.Y - (line.A.X-x); X=x} }

let lines =
    fun _ -> Console.ReadLine()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)
    |> Seq.map parseLine
    // |> Seq.filter is_horizontal_or_vertical 
    |> Seq.map normalize_if_horizontal
    |> Seq.map normalize_if_vertical
    |> Seq.map normalize_if_diagonal
    |> Seq.toList

printfn "lines=%A" lines

let points =
    lines
    |> Seq.map points_along_line
    |> Seq.toList
    |> Seq.concat
    |> Seq.groupBy id
    |> Seq.map (fun (k, v) -> (k, (Seq.length v)))
    |> Seq.toList

printfn "points=%A" points

let greater_than_two =
   points
   |> Seq.filter (fun (k, v) -> v >= 2)
   |> Seq.length

printfn ">=2 cells=%A" greater_than_two

let terminals =
    let a =
      lines
      |> Seq.map (fun l -> l.A)
    let b =
      lines
      |> Seq.map (fun l -> l.B)
    Seq.concat [a; b]
    |> Seq.toList

let minX =
  terminals
  |> Seq.map (fun p -> p.X)
  |> Seq.min

let maxX =
  terminals
  |> Seq.map (fun p -> p.X)
  |> Seq.max

let minY =
  terminals
  |> Seq.map (fun p -> p.Y)
  |> Seq.min

let maxY =
  terminals
  |> Seq.map (fun p -> p.Y)
  |> Seq.max

let pointsFreqMap =
  points
  |> Map.ofSeq

printfn "%A" (points_along_line (normalize_if_diagonal {A={X=1; Y=1}; B={X=3;Y=3}}))
printfn "%A" (points_along_line (normalize_if_diagonal {A={X=9; Y=7}; B={X=7;Y=9}}))

let draw_dots =
  for y in minX .. maxX do
    for x in minY .. maxY do
      match pointsFreqMap.TryFind {X=x; Y=y} with
      | Some(freq) -> printf "%d" freq
      | None -> printf "."
    printfn ""

draw_dots
