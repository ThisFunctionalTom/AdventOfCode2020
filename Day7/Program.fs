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

[<EntryPoint>]
let main argv =
    let bags fileName =
        readAllLines fileName
        |> Array.map parseLine
        |> Map.ofArray

    Bags.allParents "shiny gold" (bags "example.input")
    |> Set.count
    |> printfn "%A"

    Bags.allParents "shiny gold" (bags "Day7.input")
    |> Set.count
    |> printfn "%A"

    let children = Bags.allChildren "shiny gold" (bags "example.input")
    printfn "%A" children
    printfn "%A" ((Bags.count children)  - 1)

    let children = Bags.allChildren "shiny gold" (bags "Day7.input")
    printfn "%A" ((Bags.count children)  - 1)

    0 // return an integer exit code