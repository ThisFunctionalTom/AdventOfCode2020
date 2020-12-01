open System.IO

let solve1 input =
    let indexed = input |> Array.indexed
    let (_, x1), (_, x2) = 
        Array.allPairs indexed indexed
        |> Array.find (fun ((i1, x1), (i2, x2)) -> i1 <> i2 && x1 + x2 = 2020)
    x1 * x2

let solve2 input =
    let indexed = input |> Array.indexed
    let _, x1, _, x2, _, x3 = 
        Array.allPairs indexed indexed
        |> Array.allPairs indexed
        |> Array.map (fun ((i3, x3), ((i1, x1), (i2, x2))) -> i1, x1, i2, x2, i3, x3)
        |> Array.find (fun (i1, x1, i2, x2, i3, x3) -> i1 <> i2 && i2 <> i3 && i1 <> i3 && x1 + x2 + x3 = 2020)
    x1 * x2 * x3

let example = [| 1721; 979; 366; 299; 675; 1456 |]
solve1 example
solve2 example

let input = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + "/input1.txt") 
    |> Array.map int

solve1 input
solve2 input
