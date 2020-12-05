open System
open System.IO

let toRowCol (input: string) =
    let row =
        int
            ("0b"
             + input.Substring(0, 7).Replace('F', '0').Replace('B', '1'))

    let col =
        int
            ("0b"
             + input.Substring(7).Replace("R", "1").Replace("L", "0"))

    row, col

let toId (row, col) = row * 8 + col

toRowCol "FBFBBFFRLR"

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
|> Array.filter (fun (x, y) -> y - x > 1)
|> Array.map (fun (x, y) -> y - 1)