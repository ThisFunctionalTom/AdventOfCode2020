open System
open System.IO

let solve (right, down) (lines: string [])  =
    [0..down..lines.Length-1]
    |> List.filter (fun lineIdx ->
        let line = lines.[lineIdx]
        let x = (right*lineIdx/down)%line.Length
        let ch = line.[x]
        '#' = ch)
    |> List.length

let fullPath name = Path.Combine (__SOURCE_DIRECTORY__, name)

let example = File.ReadAllLines(fullPath "example.input")

solve (3, 1) example
solve (1, 2) example

let input = File.ReadAllLines(fullPath "day3.input")

solve (3, 1) input

[ 1, 1
  3, 1
  5, 1
  7, 1
  1, 2 ]
|> List.map (fun slope -> solve slope input |> bigint)
|> List.fold (*) 1I
