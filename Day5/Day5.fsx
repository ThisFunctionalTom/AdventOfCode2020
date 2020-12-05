open System.IO

let toBin zeroCh oneCh (str: string) =
    "0b"
    + str.Replace(zeroCh, '0').Replace(oneCh, '1')
    |> int

let toRowCol (input: string) =
    let row = toBin 'F' 'B' (input.Substring(0, 7))
    let col = toBin 'L' 'R' (input.Substring(7))
    row, col

let toId (row, col) = row * 8 + col

let examples =
    [| "BFFFBBFRRR", 70, 7, 567
       "FFFBBBFRRR", 14, 7, 119
       "BBFFBBFRLL", 102, 4, 820 |]

examples
|> Array.filter (fun (bp, r, c, id) ->
    let (ar, ac) = toRowCol bp
    let aid = toId (ar, ac)
    ar <> r || ac <> c || aid <> id)

File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day5.input")
|> Array.map (toRowCol >> toId)
|> Array.max

File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day5.input")
|> Array.map (toRowCol >> toId)
|> Array.sort
|> Array.pairwise
|> Array.find (fun (x, y) -> y - x <> 1)
|> fun (x, y) -> y - 1
