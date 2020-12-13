open System.IO

let readInput fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map int

let prepare input =
    let max = Array.max input
    Array.append [| 0; max+3 |] input
    |> Array.sort

let solve1 input =
    let cnt =
        prepare input
        |> Array.pairwise
        |> Array.map (fun (x,y) -> y - x)
        |> Array.takeWhile (fun d -> d <= 3)
        |> Array.countBy id
        |> Map.ofArray

    let get diff = cnt |> Map.tryFind diff |> Option.defaultValue 0
    
    (get 1) * (get 3)

solve1 <| readInput "example1.input"
solve1 <| readInput "example2.input"
solve1 <| readInput "Day10.input"

let input = 
    readInput "Day10.input"
    |> Array.sort

Array.pairwise input
|> Array.takeWhile (fun (x, y) -> y-x <= 3)

let split (input: int[]) =
    let rec loop (start,curr) =
        [| let next = curr + 1
           if next = input.Length 
           then yield input.[start..curr]
           else
            let diff = input.[next] - input.[curr]
            match diff with
            | 1 | 2 -> yield! loop (start, next)
            | 3 -> 
                yield input.[start..curr]
                yield! loop (next, next)
            | _ -> failwith "Ooopsi" |]
    loop (0, 0)

split (readInput "example2.input" |> prepare)

let solve2 input =
    prepare input
    |> split
    |> Array.map (fun g ->
        match g.Length with
        | 1 | 2 -> 1UL
        | 3 -> 2UL
        | 4 -> 4UL
        | 5 -> 7UL
        | x -> failwithf "Not implemented %d" x )
    |> Array.reduce (*)

readInput "Day10.input" |> solve2

let solve2better fileName =
    let input = 
        readInput fileName
        |> Array.append [|0|]
        |> Array.sortDescending
        |> List.ofArray

    let max = input.[0]

    input
    |> List.fold (fun comb next ->
        let nc = 
            comb 
            |> List.filter (fun (v, _) -> v - next <= 3)
            |> List.sumBy snd
        (next, nc)::comb
    ) [max+3, 1UL]
    |> List.head
    |> snd

solve2better "example1.input" |> printfn "%d"
solve2better "example2.input" |> printfn "%d"
solve2better "Day10.input" |> printfn "%d" // 1322306994176
            

