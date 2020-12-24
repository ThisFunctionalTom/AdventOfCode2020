open System
open System.IO
open System

module String =
    let normalizeNewLine (str: string) =
        str.Replace("\r\n", "\n")
    let split (sep: string) (input: string) =
        input.Split(sep)
    let reverse (str: string) =
        str.ToCharArray()
        |> Array.rev
        |> String

let nl = "\n"

type Tile = {
    Id: int
    Orientations: int[][] 
} with
    member tile.Borders = 
        let allNormal = tile.Orientations.[0]
        let allReversed = tile.Orientations.[2]
        Array.append allNormal allReversed

let orientations (borders: int32[][]) =
    let b nr = borders.[nr].[0]
    let revb nr = borders.[nr].[1]
    [| 
        [| b 0; b 1; b 2; b 3 |]
        [| revb 3; b 0; revb 1; b 2 |]
        [| revb 2; revb 3; revb 0; revb 1 |]
        [| b 1; revb 2; b 3; revb 0 |]
        [| b 3; b 2; b 1; b 0 |]
        [| revb 0; b 3; revb 2; b 1 |]
        [| revb 1; revb 0; revb 3; revb 2 |]
        [| b 2; revb 1; b 0; revb 3 |]
    |]

let parseTile (str: string) =
    let toNumber (border: string) =
        let binaryStr = border.Replace("#", "1").Replace(".", "0")
        let revBinaryStr = binaryStr |> String.reverse
        [| Convert.ToInt32(binaryStr, fromBase=2); Convert.ToInt32(revBinaryStr, fromBase=2) |]
    
    let lines = str.Split(nl)
    
    let tileIdStr = lines.[0].Split(' ').[1].[0..^1]
    let tileId = tileIdStr |> int
    let borders =
        [| lines.[1]
           lines.[^0]
           lines.[1..] |> Array.map (fun line -> line.[0]) |> String
           lines.[1..] |> Array.map (fun line -> line.[^0]) |> String |]
        |> Array.map toNumber
    { Id = tileId; Orientations = orientations borders }
    
let readInput fileName =
    Path.Combine (__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllText
    |> String.normalizeNewLine
    |> String.split $"{nl}{nl}"
    |> Array.map parseTile

let isPair (tile1: Tile) (tile2: Tile) =
    tile1.Borders
    |> Array.exists (fun b -> tile2.Borders |> Array.contains b)

let getPairs (tiles: Tile []) =
    [| for i in 0..tiles.Length-1 do 
            for j in i+1..tiles.Length-1 do
                if isPair tiles.[i] tiles.[j] 
                then tiles.[i].Id, tiles.[j].Id |]

let solve1 tiles =
    getPairs tiles
    |> Array.collect (fun (t1, t2) -> [| t1; t2 |])
    |> Array.countBy id
    |> Array.filter (snd >> (=) 2)
    |> Array.map (fst >> uint64)
    |> Array.reduce (*)

readInput "example.input" |> solve1

readInput "Day20.input" |> solve1
