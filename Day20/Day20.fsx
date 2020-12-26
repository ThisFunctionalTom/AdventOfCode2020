open System
open System.IO

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

module Border =
    let toBinString (i: int) =
        let binStr = Convert.ToString(i, toBase=2)
        String('0', 10 - binStr.Length) + binStr

    let toString (i: int) =
        (toBinString i).Replace('0', '.').Replace('1', '#')

    let fromBinString (binStr: string) =
        Convert.ToInt32(binStr, fromBase=2)

    let fromString (str: string) =
        str.Replace('#', '1').Replace('.', '0')
        |> fromBinString

    let reverse (i: int) =
        toBinString i
        |> String.reverse
        |> fromBinString

module Lines =
    let transpose (lines: string[]) =
        lines 
        |> Array.map Array.ofSeq 
        |> Array.transpose 
        |> Array.map String

    let flipVertical (lines: string[]) =
        Array.rev lines

    let flipHorizontal (lines: string[]) =
        lines |> Array.map String.reverse

    let show (lines: string[]) =
        lines
        |> Array.iter (printfn "%s")

type Tile = {
    Id: int
    Lines: string []
} with
    member t.PictureLines =
        t.Lines.[1..^1]
        |> Array.map (fun line -> line.[1..^1])
    
    member t.FlipVertical =
        { Id = t.Id; Lines = Lines.flipVertical t.Lines }

    member t.FlipHorizontal =
        { Id = t.Id; Lines = Lines.flipHorizontal t.Lines }

    member t.Transpose =
        { t with Lines = Lines.transpose t.Lines }

    member t.Borders =
        [| t.Lines.[0]
           t.Lines |> Array.map (fun line -> line.[^0]) |> String
           t.Lines.[^0]
           t.Lines |> Array.map (fun line -> line.[0]) |> String |]
        |> Array.map Border.fromString

    member t.TopBorder = t.Borders.[0]
    member t.RightBorder = t.Borders.[1]
    member t.BottomBorder = t.Borders.[2]
    member t.LeftBorder = t.Borders.[3]

    member t.AllBorders =
        let borders = t.Borders
        Array.append borders (borders |> Array.map Border.reverse)

    static member FromString (str: string) =
        let lines = str.Split(nl)
        let tileIdStr = lines.[0].Split(' ').[1].[0..^1]
        let tileId = tileIdStr |> int
        { Id = tileId; Lines = lines.[1..] }

module Tile =
    let id (tile: Tile) = tile.Id

fsi.AddPrinter(fun (t: Tile) -> 
    [ $"Tile: {t.Id}"
      yield! t.Lines ]
    |> String.concat "\r\n"
)

module Tiles =
    let get tileId (tiles: Tile[]) = 
        tiles 
        |> Array.find (fun t -> t.Id = tileId)

    let remove tileId (tiles: Tile[]) =
        tiles
        |> Array.filter (fun t -> t.Id <> tileId)

let readInput fileName =
    Path.Combine (__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllText
    |> String.normalizeNewLine
    |> String.split $"{nl}{nl}"
    |> Array.map Tile.FromString

let isPair (tile1: Tile) (tile2: Tile) =
    tile1.Id <> tile2.Id
    && 
        Array.allPairs tile1.Borders tile2.AllBorders
        |> Array.exists (fun (b1, b2) -> b1 = b2)

let getPairs (tiles: Tile []) =
    [| for i in 0..tiles.Length-1 do 
            for j in i+1..tiles.Length-1 do
                if isPair tiles.[i] tiles.[j] 
                then tiles.[i], tiles.[j] |]

let getCorners (tiles: Tile[]) =
    getPairs tiles
    |> Array.collect (fun (t1, t2) -> [| t1; t2 |])
    |> Array.countBy id
    |> Array.filter (snd >> (=) 2)
    |> Array.map fst

let solve1 tiles =
    getCorners tiles
    |> Array.map (Tile.id >> uint64)
    |> Array.reduce (*)

readInput "example.input" |> solve1
readInput "Day20.input" |> solve1

let getTopLeftCorner (tiles: Tile[]) =
    let corners = getCorners tiles
    let corner = corners.[0] // Take any corner
    let outerBorders =
        let hasMatchingTile border =
            tiles
            |> Array.exists (fun t -> t.Id <> corner.Id && t.AllBorders |> Array.contains border)
        corner.Borders
        |> Array.indexed
        |> Array.filter (fun (_idx, border) -> not (hasMatchingTile border))
        |> Array.map fst

    printfn $"%A{outerBorders}"
    match outerBorders with
    | [| 0; 1 |] -> corner.FlipHorizontal
    | [| 1; 2 |] -> corner.FlipHorizontal.FlipVertical
    | [| 2; 3 |] -> corner.FlipVertical
    | [| 0; 3 |] -> corner
    | _ -> failwith "OOopsi"

let transformToTop (tile: Tile) (borderIndex: int) =
    match borderIndex with
    | 0 -> tile
    | 1 -> tile.FlipHorizontal.Transpose
    | 2 -> tile.FlipVertical
    | 3 -> tile.Transpose
    | 4 -> tile.FlipHorizontal
    | 5 -> tile.FlipHorizontal.FlipVertical.Transpose
    | 6 -> tile.FlipHorizontal.FlipVertical
    | 7 -> tile.FlipVertical.Transpose
    | _ -> failwith $"Invalid border index {borderIndex}"

let transformToLeft (tile: Tile) (borderIndex: int) = 
    (transformToTop tile borderIndex).Transpose

let tryFindBorder (border: int) (tiles: Tile []) =
    tiles
    |> Array.tryPick (fun t ->
        t.AllBorders 
        |> Array.tryFindIndex ((=) border)
        |> Option.map (fun bidx -> t, bidx))

let getRow (leftTile: Tile) (tiles: Tile[])  =
    let rec loop (row: Tile list) (rest: Tile[]) =
        let lastTile = List.head row
        let nextOpt = tryFindBorder lastTile.RightBorder rest
        match nextOpt with
        | None -> row |> Array.ofList |> Array.rev, rest
        | Some (next, bidx) -> loop (transformToLeft next bidx::row) (Tiles.remove next.Id rest)

    loop [leftTile] tiles

let orderTiles (tiles: Tile[]) =
    let rec loop (rows: Tile[] list) (rest: Tile[]) =
        if Array.isEmpty rest 
        then rows |> Array.ofList |> Array.rev
        else
            let firstTile =
                match rows with
                | [] -> getTopLeftCorner tiles
                | lastRow::_ ->
                    let topBorder = lastRow.[0].BottomBorder
                    let (tile, bidx) = tryFindBorder topBorder rest |> Option.get
                    transformToTop tile bidx 
            let row, rest = getRow firstTile (Tiles.remove firstTile.Id rest)
            loop (row::rows) rest

    loop [] tiles

let getPicture (tiles: Tile[][]) =
    tiles 
    |> Array.collect (fun row ->
        row
        |> Array.map (fun t -> t.PictureLines)
        |> Array.reduce (fun l1 l2 -> 
            Array.zip l1 l2
            |> Array.map (fun (s1, s2) -> s1 + s2)))

type String with
    member str.AllIndexesOf (search: string) =
        let rec loop indexes =
            let startIdx =
                indexes
                |> List.tryHead
                |> Option.map ((+) 1)
                |> Option.defaultValue 0

            let nextIdx = str.IndexOf(search, startIdx)
            match nextIdx with
            | -1 -> indexes |> List.rev
            | idx -> loop (idx::indexes)
        loop []

let monster = """
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
"""

let monsterCoords =
    monster.Replace("\r\n", "\n").Split("\n") 
    |> Array.filter (not << String.IsNullOrEmpty)
    |> Array.indexed
    |> Array.collect (fun (lineIdx, line) ->
        line.AllIndexesOf("#")
        |> Array.ofList
        |> Array.map (fun lineOffset -> lineIdx, lineOffset))

let getMonsterCoords (line, offset) =
    monsterCoords
    |> Array.map (fun (lineOff, offsetOff) -> line+lineOff, offset+offsetOff)

let isMatch (picture: string[]) monsterCoords =
    monsterCoords
    |> Array.forall (fun (x, y) -> picture.[x].[y] = '#')

let getSeaMonsterCoords (picture: string[]) =
    let maxLine = picture.Length - (monsterCoords |> Array.map fst |> Array.max)
    let maxOffset = picture.[0].Length - (monsterCoords |> Array.map snd |> Array.max)
    
    Array.allPairs [| 0..maxLine-1 |] [| 0..maxOffset-1 |]
    |> Array.collect (fun (line, offset) ->
        let coords = getMonsterCoords (line, offset)
        if isMatch picture coords then coords else Array.empty)

let getWaterRoughness picture =
    let monsterHashes =
        [| id; Lines.flipHorizontal
           Lines.flipVertical; Lines.flipVertical >> Lines.flipHorizontal
           Lines.transpose; Lines.transpose >> Lines.flipVertical
           Lines.transpose >> Lines.flipHorizontal; Lines.transpose >> Lines.flipVertical >> Lines.flipHorizontal |]
        |> Array.map (fun f -> f picture |> getSeaMonsterCoords)
        |> Array.maxBy (fun coords -> coords.Length)
        |> Array.distinct
        |> Array.length
    let allHashes =
        picture
        |> Array.sumBy (Seq.filter (fun c -> c = '#') >> Seq.length)
    allHashes - monsterHashes

let solve2 fileName =
    let tiles = readInput fileName
    let picture = orderTiles tiles |> getPicture
    getWaterRoughness picture

solve2 "example.input"
solve2 "Day20.input"