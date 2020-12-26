open System
open System.IO

let parseLine (line: string) =
    let [| ingredientsStr; alergensStr |] = line.Split(" (contains ")
    ingredientsStr.Split(' ') |> Set.ofArray, alergensStr.TrimEnd(')').Split(", ") |> Set.ofArray

let readInput fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map parseLine

let solve1 fileName =
    let example = readInput fileName
    let possibleIngs =
        example
        |> Array.collect (fun (ings, algs) ->
            algs
            |> Set.toArray
            |> Array.map (fun alg -> alg, ings))
        |> Array.groupBy fst
        |> Array.map (fun (alg, arr) -> 
            alg, arr |> Array.map snd |> Array.reduce Set.intersect)
        |> Array.collect (snd >> Set.toArray)
        |> Set.ofArray

    let allIngs = 
        example 
        |> Array.collect (fst >> Set.toArray) 
        |> Set.ofArray

    let noAlgs = Set.difference allIngs possibleIngs

    example
    |> Array.sumBy (fun (ings, algs) -> Set.intersect ings noAlgs |> Set.count)

solve1 "example.input"
solve1 "Day21.input"