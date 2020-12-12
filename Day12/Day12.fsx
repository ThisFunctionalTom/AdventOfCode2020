open System
open System.IO

let readInput fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map (fun line -> line.[0], int (line.[1..]))

let rotate (directions: char []) (dir: char) (value: int)  =
    let dirIdx = directions |> Array.findIndex ((=) dir)
    let valIdx = value%360/90
    let newIdx = (dirIdx + valIdx) % directions.Length
    directions.[newIdx]

let rotateRight = rotate [|'N'; 'E'; 'S'; 'W'|]
let rotateLeft = rotate [|'N'; 'W'; 'S'; 'E'|]

rotateLeft 'E' 90
rotateRight 'E' 90

let move (coord, dir) (char, value) =
    let (+) (x1, y1) (x2, y2) = x1+x2, y1 + y2 
    
    match dir, char with
    | _, 'N' | 'N', 'F' -> coord + (0, value), dir
    | _, 'S' | 'S', 'F' -> coord + (0, -value), dir
    | _, 'E' | 'E', 'F' -> coord + (value, 0), dir
    | _, 'W' | 'W', 'F' -> coord + (-value, 0), dir
    | _, 'L' -> coord, rotateLeft dir value
    | _, 'R' -> coord, rotateRight dir value
    | _ -> failwithf "invalid direction: %c %d" char value

let solve input =
    input 
    |> Array.fold move ((0, 0), 'E')
    |> fun ((x, y), _) -> abs x + abs y
            
readInput "example.input" |> solve
readInput "Day12.input" |> solve

let rotateWp (wpx, wpy) value =
    match value%360 with
    | 0 -> wpx, wpy
    | 90  | -270 -> wpy, -wpx
    | 180 | -180 -> -wpx, -wpy
    | 270 | -90 -> -wpy, wpx
    | _ -> failwithf "Invalid rotation %d" value

let move2 (coord, wp) (char, value) =
    let (+) (x1, y1) (x2, y2) = x1+x2, y1 + y2 
    let (*) n (x, y) = n*x, n*y
    
    match char with
    | 'F' -> coord + value * wp, wp
    | 'N' -> coord, wp + (0, value)
    | 'S' -> coord, wp + (0, -value)
    | 'E' -> coord, wp + (value, 0)
    | 'W' -> coord, wp + (-value, 0)
    | 'R' -> coord, rotateWp wp value
    | 'L' -> coord, rotateWp wp -value
    | _ -> failwithf "invalid direction: %c %d" char value
let solve2 input =
    input
    |> Array.fold move2 ((0, 0), (10, 1))
    |> fun ((x, y), _) -> abs x + abs y

readInput "example.input" |> solve2
readInput "Day12.input" |> solve2
