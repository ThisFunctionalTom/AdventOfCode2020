open System
open System.IO

let parseLine (line: string) =
    let [| ingredientsStr; alergensStr |] = line.Split(" (contains ")
    ingredientsStr.Split(' ') |> Set.ofArray, alergensStr.TrimEnd(')').Split(", ") |> Set.ofArray

let readInput fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map parseLine

let getMappings (input: (Set<string>*Set<string>)[]) =
    input
    |> Array.collect (fun (ings, algs) ->
        algs
        |> Set.toArray
        |> Array.map (fun alg -> alg, ings))
    |> Array.groupBy fst
    |> Array.map (fun (alg, arr) -> 
        alg, arr |> Array.map snd |> Array.reduce Set.intersect)

let solve1 fileName =
    let input = readInput fileName
    let mappings = getMappings input

    let possibleIngs =
        mappings
        |> Array.collect (snd >> Set.toArray)
        |> Set.ofArray

    let allIngs = 
        input 
        |> Array.collect (fst >> Set.toArray) 
        |> Set.ofArray

    let noAlgs = Set.difference allIngs possibleIngs

    input
    |> Array.sumBy (fun (ings, algs) -> Set.intersect ings noAlgs |> Set.count)

solve1 "example.input"
solve1 "Day21.input"

let solveMappings (input: (string*Set<string>)[]) =
    let rec loop (algs: (string*Set<string>) []) =
        let single =
            algs
            |> Array.map snd
            |> Array.filter (Set.count >> ((=) 1))
            |> Array.collect Set.toArray
            |> Set.ofArray

        printfn $"Found: {single.Count}/algs.Length"
        if single.Count = algs.Length 
        then algs |> Array.map (fun (alg, ings) -> alg, Set.toArray ings |> Array.head)
        else 
            algs
            |> Array.map (fun (alg, ings) ->
                if Set.count ings = 1 
                then alg, ings
                else alg, Set.difference ings single)
            |> loop

    loop input

let solve2 fileName =
    let mappings =
        readInput fileName
        |> getMappings
        |> solveMappings
    
    mappings
    |> Array.sortBy fst
    |> Array.map snd
    |> String.concat ","

solve2 "example.input"
solve2 "Day21.input"