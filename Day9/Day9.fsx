open System.IO

let readInput fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map uint64

let pairs (arr: uint64[]) =
    Array.allPairs arr arr
    |> Array.filter (fun (x, y) -> x <> y)

let solve1 preamble input =
    input
    |> Array.windowed (preamble + 1)
    |> Array.pick (fun arr -> 
        let n = arr.[preamble]
        let found = 
            pairs arr.[0..preamble-1]
            |> Array.exists (fun (x, y) -> x + y = n)
        if not found then Some n else None)

let solve2 (input: uint64[]) (n: uint64) =
    let rec loop (b, e) =
        let range = input.[b..e]
        let x = Array.sum range
        if x = n 
        then Array.min range + Array.max range
        elif x > n
        then if b = e-1 then loop (b+1, e+1) else loop (b+1, e)
        else loop (b, e+1)
    loop (0, 1) 

readInput "example.input" |> solve1 5
readInput "Day9.input" |> solve1 25
let solution1 = 26796446UL

let input = readInput "Day9.input"
let n = solve1 25 input
solve2 input n

