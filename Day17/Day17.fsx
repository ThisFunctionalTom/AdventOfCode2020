open System
open System.IO

let readInput fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.indexed
    |> Array.collect (fun (y, chars) ->
        chars
        |> Array.ofSeq
        |> Array.indexed
        |> Array.filter (fun (x, c) -> c = '#' )
        |> Array.map (fun (x, c) -> x, y, 0)
    )
    |> Set.ofArray

let neighbours (x, y, z) =
    [| for i in x-1..x+1 do
        for j in y-1..y+1 do
            for k in z-1..z+1 do
                i, j, k |]
    |> Set.ofArray
    |> Set.remove (x, y, z)

let countNeighbours item s =
    neighbours item 
    |> Set.filter (fun x -> Set.contains x s) 
    |> Set.count

let getRange s =
    let (sx, sy, sz) = Set.minElement s
    s 
    |> Set.fold (fun ((minx, maxx), (miny, maxy), (minz, maxz)) (x, y, z) -> 
        (min minx x, max maxx x), (min miny y, max maxy y), (min minz z, max maxz z)) 
        ((sx, sx), (sy, sy), (sz, sz))

let show s =
    let (minx, maxx), (miny, maxy), (minz, maxz) = getRange s 
    [ for z in minz..maxz do
        $"z={z}"
        for y in miny..maxy do
            [| for x in minx..maxx do 
                if Set.contains (x, y, z) s then '#' else '.' |]
            |> String
        "" ]
    |> String.concat "\n"

let showNeighbours s =
    let (minx, maxx), (miny, maxy), (minz, maxz) = getRange s 
    [ for z in minz..maxz do
        $"z={z}"
        for y in miny..maxy do
            [| for x in minx..maxx do 
                let isActive = if Set.contains (x, y, z) s then '#' else '.'
                let neighbours = countNeighbours (x, y, z) s
                sprintf "%c%02d" isActive neighbours |]
            |> String.concat " "
        "" ]
    |> String.concat "\n"

let solve iterations s =
    let rec loop iteration s =
        if iteration = iterations 
        then s
        else
            let allNeighbours =
                s 
                |> Set.fold (fun acc n -> Set.union acc (neighbours n)) Set.empty

            Set.union s allNeighbours
            |> Set.filter (fun ((x, y, z) as coord) -> 
                let isActive = Set.contains coord s
                let neighbours = countNeighbours coord s
                match isActive, neighbours with
                | true, (2|3) -> true
                | false, 3 -> true
                | _ -> false)
            |> loop (iteration + 1)
    loop 0 s

neighbours (0, 0, 0)
|> showNeighbours

readInput "example.input"
|> solve 6
|> Set.count

readInput "Day17.input"
|> solve 6
|> Set.count

let neighbours2 (x, y, z, w) =
    [| for i in x-1..x+1 do
        for j in y-1..y+1 do
            for k in z-1..z+1 do
                for l in w-1..w+1 do
                    i, j, k, l |]
    |> Set.ofArray
    |> Set.remove (x, y, z, w)

let solve2 iterations s =
    let rec loop iteration s =
        if iteration = iterations 
        then s
        else
            let allNeighbours =
                s 
                |> Set.fold (fun acc n -> Set.union acc (neighbours2 n)) Set.empty

            Set.union s allNeighbours
            |> Set.filter (fun coord -> 
                let isActive = Set.contains coord s
                let neighbours = neighbours2 coord |> Set.filter s.Contains |> Set.count
                match isActive, neighbours with
                | true, (2|3) -> true
                | false, 3 -> true
                | _ -> false)
            |> loop (iteration + 1)
    loop 0 s

readInput "example.input"
|> Set.map (fun (x, y, z) -> x, y, z, 0)
|> solve2 6
|> Set.count

readInput "Day17.input"
|> Set.map (fun (x, y,  z) -> x, y, z, 0)
|> solve2 6
|> Set.count