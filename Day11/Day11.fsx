open System
open System.IO

let readInput fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map Array.ofSeq

let rows (arr: 'a [] []) = arr.Length
let cols (arr: 'a [] []) = arr.[0].Length

let allIdxs (arr: char [] []) =
    [| for i in [ 0 .. rows arr - 1 ] do
        for j in [ 0 .. cols arr - 1 ] do
            i, j |]

let neighbours (seats: char [] []) (r, c) =
    let rows = rows seats
    let cols = cols seats

    let idxs =
        let validRows =
            [ max 0 (r - 1) .. min (r + 1) (rows - 1) ]

        let validCols =
            [ max 0 (c - 1) .. min (c + 1) (cols - 1) ]

        List.allPairs validRows validCols 
        |> List.filter (fun (i, j) -> not (i = r && j = c)) // remove x, y from neighbours

    idxs
    |> List.sumBy (fun (i, j) -> if seats.[i].[j] = '#' then 1 else 0)

let next (seats: char [] []) =
    [| for i in [ 0 .. rows seats - 1 ] do
        [| for j in [ 0 .. cols seats - 1 ] do
            match seats.[i].[j] with
            | 'L' when neighbours seats (i, j) = 0 -> '#'
            | '#' when neighbours seats (i, j) >= 4 -> 'L'
            | x -> x |] |]

let countOccupied (seats: char [] []) =
    Array.sumBy (Array.sumBy (fun c -> if c = '#' then 1 else 0)) seats

let rec solve (seats: char [] []) =
    let n = next seats
    if n = seats then countOccupied seats else solve n

readInput "example.input" |> solve

readInput "Day11.input" |> solve

let neighbours2 (seats: char [] []) (r, c) =
    let rows = rows seats
    let cols = cols seats
    let withRow r c = r, c
    let withCol c r = r, c

    let idxs (rd, cd) =
        List.unfold (fun (i, j) ->
            let ni = i+rd
            let nj = j+cd
            if ni < 0 || nj < 0 || ni >= rows || nj >= cols 
            then None
            else Some((ni, nj), (ni, nj))) (r, c)

    let rec isOccupied idxs =
        match idxs with
        | [] -> false
        | (i, j)::rest ->
            if seats.[i].[j] = 'L' 
            then false
            elif seats.[i].[j] = '#'
            then true
            else isOccupied rest

    [ -1,-1
      -1,0
      -1,1
      0,-1
      0,1
      1,-1
      1,0
      1,1 ]
    |> List.filter (idxs >> isOccupied)
    |> List.length

let next2 (seats: char [] []) =
    [| for i in [ 0 .. rows seats - 1 ] do
        [| for j in [ 0 .. cols seats - 1 ] do
            match seats.[i].[j] with
            | 'L' when neighbours2 seats (i, j) = 0 -> '#'
            | '#' when neighbours2 seats (i, j) >= 5 -> 'L'
            | x -> x |] |]

let rec solve2 (seats: char [] []) =
    let n = next2 seats
    if n = seats then countOccupied seats else solve2 n

readInput "example.input" |> solve2
readInput "Day11.input" |> solve2