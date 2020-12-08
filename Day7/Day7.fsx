open System.IO

let readAllLines fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines

let parseLine (line: string) =
    let bagColor (str: string) = str.[..str.IndexOf(" bag")-1]

    let parseChild (str: string) =
        let [| countStr; colorStr |] = str.Split(" ", 2)
        int countStr, bagColor colorStr

    let [| parentStr; childrenStr |] = line.Split("contain ")
    let children = 
        if childrenStr = "no other bags."
        then []
        else
            childrenStr.Split(", ")
            |> Array.map parseChild
            |> List.ofArray
    bagColor parentStr, children

type Color = string

type Bags = Map<Color, List<int*Color>>

module Bags =
    let hasChild color (children: List<int*Color>) =
        children
        |> List.exists (fun (_, c) -> c = color)

    let allParents color (bags: Bags) =
        let getParents bagColor =
            bags 
            |> Map.filter (fun p ch -> hasChild bagColor ch)
            |> Map.toList
            |> List.map fst
            |> Set.ofList

        let rec loop acc parents =
            let newParents = Set.difference parents acc
            if Set.isEmpty newParents
            then acc
            else
                newParents
                |> Set.fold (fun s p -> Set.union s (getParents p)) Set.empty
                |> loop (Set.union acc parents)
        
        loop Set.empty (getParents color)

    let allChildren color (bags: Bags) =
        let getChildren color =
            Map.tryFind color bags
            |> Option.defaultValue List.empty

        let rec loop (cnt, color) =
            [ yield cnt, color
              for ccnt, ccolor in getChildren color  do
                yield! loop (cnt*ccnt, ccolor) ]

        loop (1, color)

    let count (bags: List<int*Color>) =
        bags |> List.sumBy fst

    let readFromFile fileName =
        readAllLines fileName
        |> Array.map parseLine
        |> Map.ofArray

let example = Bags.readFromFile "example.input"
let input = Bags.readFromFile "Day7.input"

Bags.allParents "shiny gold" example
|> Set.count
|> printfn "%A"

Bags.allParents "shiny gold" input
|> Set.count
|> printfn "%A"

let exampleChildren = Bags.allChildren "shiny gold" example
printfn "%A" exampleChildren
printfn "%A" ((Bags.count exampleChildren)  - 1)

let inputChildren = Bags.allChildren "shiny gold" input
printfn "%A" ((Bags.count inputChildren)  - 1)