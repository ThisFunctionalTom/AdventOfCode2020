open System
open System.IO

let readInput fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines

let parseLine (line: string) =
    " " + line
    |> Seq.pairwise
    |> Seq.choose
        (fun (c1, c2) ->
            match c1, c2 with
            | ' ', 'e'
            | ' ', 'w' -> Some(String([| c2 |]))
            | 'n', _
            | 's', _ -> Some(String([| c1; c2 |]))
            | _, 'e'
            | _, 'w' -> Some(String([| c2 |]))
            | _ -> None)
    |> List.ofSeq

let getPosition (directions: string list) =
    let map =
        directions |> List.countBy id |> Map.ofList

    let get key =
        map |> Map.tryFind key |> Option.defaultValue 0

    let x =
        2 * (get "e") - 2 * (get "w")
        + (get "se")
        + (get "ne")
        - (get "sw")
        - (get "nw")

    let y =
        (get "ne") + (get "nw") - (get "se") - (get "sw")

    x, y

let getBlackTiles lines =
    lines
    |> Array.countBy (parseLine >> getPosition)
    |> Array.filter (snd >> (fun count -> count % 2 <> 0))
    |> Array.map fst
    |> Set.ofArray

let solve1 fileName =
    readInput fileName |> getBlackTiles |> Set.count

solve1 "example.input"
solve1 "Day24.input"

let getNeighbours (x, y) =
    [| -2, 0 // w
       -1, 1 // nw
       1, 1 // ne
       2, 0 // e
       1, -1 // se
       -1, -1 |] // se
    |> Array.map (fun (x2, y2) -> x + x2, y + y2)
    |> Set.ofArray

module Set =
    let collect f s = s |> Seq.collect f |> Set.ofSeq

let flip (blackTiles: Set<int * int>) =
    blackTiles
    |> Set.collect getNeighbours
    |> Set.union blackTiles
    |> Set.filter
        (fun coord ->
            let isBlack = blackTiles.Contains coord

            let blackNeighbours =
                getNeighbours coord
                |> Set.filter blackTiles.Contains
                |> Set.count

            (isBlack
             && (blackNeighbours = 1 || blackNeighbours = 2))
            || (not isBlack && blackNeighbours = 2))

let solve2 fileName =
    readInput fileName
    |> getBlackTiles
    |> Seq.unfold (fun state -> Some(state.Count, flip state))
    |> Seq.take 101
    |> Seq.last

solve2 "example.input"
solve2 "Day24.input"
