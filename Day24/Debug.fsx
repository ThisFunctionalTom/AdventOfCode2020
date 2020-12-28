#load "Day24.fsx"
open Day24

let blue = "\u001b[34m"
let yellow = "\u001b[33m"
let white = "\u001b[37m"
let reset = "\u001b[0m"
let cyan = "\u001b[36m"
let magenta = "\u001b[35m"
let green = "\u001b[32m"

let getTileCell (blackTiles: Set<int*int>) (i, j) =
    if blackTiles.Contains (i, j)
    then $"{blue}#"
    else $"{yellow}."

let getNeighbourCell (blackTiles: Set<int*int>) (i, j) =
    let isBlack = blackTiles.Contains (i, j)
    let blackNeighbours =
        getNeighbours (i, j)
        |> Set.filter blackTiles.Contains
        |> Set.count
    let color = 
        match isBlack, blackNeighbours with
        | true, (1|2) -> blue
        | true, _ -> magenta
        | false, 2 -> green 
        | false, _ -> yellow
    if blackNeighbours = 0 
    then $"{color}."
    else $"{color}{blackNeighbours}"

let show (getCell: int*int -> string) (minx, maxx) (miny, maxy) =
    let xCoords =
        [minx..maxx] 
        |> List.map (fun i -> 
            if i = 0 then "|"
            elif abs(i % 10) = 0 then "0"
            elif abs(i % 5) = 0 then "5"
            else " ")
        |> String.concat ""
    [ $"{reset}__: {xCoords}"
      for i in maxy .. -1 .. miny do
        let row = 
            [ for j in minx..maxx do 
                let isSpace = abs(i%2) <> abs(j%2)
                if isSpace 
                then " "
                else getCell (i, j) ] 
            |> String.concat "" 
        $"{reset}%02d{i}: {row}"
    ]
    |> List.iter (printfn "%s")

module Arrow =
    let w = "⭠"
    let e = "⭢"
    let nw = "⭦"
    let ne = "⭧"
    let se = "⭨"
    let sw = "⭩"

let toMovement dir =
    match dir with
    | "w" -> 0, -2
    | "nw" -> 1, -1
    | "sw" -> -1, -1
    | "e" -> 0, 2
    | "ne" -> 1, 1
    | "se" -> -1, 1
    | _ -> failwith $"Invalid direction {dir}"

let toArrow dir =
    match dir with
    | "w" -> Arrow.w
    | "nw" -> Arrow.nw
    | "sw" -> Arrow.sw
    | "e" -> Arrow.e
    | "ne" -> Arrow.ne
    | "se" -> Arrow.se
    | _ -> failwith $"Invalid direction {dir}"

let getPath (directions: string list) =
    let add (x1, y1) (x2, y2) = x1+x2, y1+y2
    
    let rec folder (pos, directions: string list) =
        match directions with
        | [] -> Some ((pos, "#"), (pos, []))
        | dir::rest ->
            let movement, arrow =
                match dir with
                | "w" -> (0, -2), Arrow.w
                | "nw" -> (1, -1), Arrow.nw
                | "sw" -> (-1, -1), Arrow.sw
                | "e" -> (0, 2), Arrow.e
                | "ne" -> (1, 1), Arrow.ne
                | "se" -> (-1, 1), Arrow.se
                | _ -> failwith $"Invalid direction {dir}"
            Some ((pos, arrow), (add pos movement, rest))
    
    List.unfold folder ((0, 0), directions)
    |> Map.ofList

let getPathCell (path: Map<int*int, string>) (i, j) =
    match path |> Map.tryFind (i, j) with
    | Some str -> str
    | None -> "."

let show20x20 tiles = show (getTileCell tiles) (-10, 10) (-10, 10)
let show20x20nb tiles = show (getNeighbourCell tiles) (-10, 10) (-10, 10)
let show20x20p path = show (getPathCell path) (-10, 10) (-10, 10)

let showPath (directions: string list) =
    for i in 1..directions.Length do
        let dirs = directions |> List.take i
        let path = getPath dirs
        printfn $"*** %A{dirs} ***"
        show20x20p path

readInput "example.input"
|> getBlackTiles
|> show20x20nb

solve2 "example.input"
solve2 "Day24.input"

