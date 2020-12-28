open System
open System.IO

module Map =
    let getValueOrDefault key defaultValue =
        Map.tryFind key
        >> Option.defaultValue defaultValue

let readInput fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines

let parseLine (line: string) =
    " " + line
    |> Seq.pairwise
    |> Seq.choose (fun (c1, c2) ->
        match c1, c2 with
        | ' ', ('e'| 'w') -> Some (String([|c2|]))
        | ('n'|'s'), _ -> Some (String([|c1; c2|]))
        | _, ('e' | 'w') -> Some (String([|c2|]))
        | _ -> None)
    |> List.ofSeq

let getPosition (directions: string list) =
    let map =
        directions
        |> List.countBy id
        |> Map.ofList

    let get key = map |> Map.tryFind key |> Option.defaultValue 0

    let x = 2*(get "e") - 2*(get "w") + (get "se") + (get "ne") - (get "sw") - (get "nw")
    let y = (get "ne") + (get "nw") - (get "se") - (get "sw")
    x, y

let solve1 fileName =
    readInput fileName
    |> Array.countBy (parseLine >> getPosition)
    |> Array.filter (snd >> (fun count -> count % 2 <> 0))
    |> Array.length

solve1 "example.input"
solve1 "Day24.input"