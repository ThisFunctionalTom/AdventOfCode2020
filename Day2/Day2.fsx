open System
open System.IO
open System.Text.RegularExpressions

type Password =
    { Min: int
      Max: int
      Char: char
      Password: string }

let parseLine (line: string) =
    let re =
        Regex("(?<min>\d+)[-](?<max>\d+)\s+(?<char>[^:]):\s+(?<pwd>.*)")

    let m = re.Match(line)
    let gval (name: string) = m.Groups.[name].Value
    if m.Success then
        Some
            { Min = int (gval "min")
              Max = int (gval "max")
              Char = (gval "char").[0]
              Password = gval "pwd" }
    else
        None

let isValid1 (pwd: Password) =
    let charCounts = 
        pwd.Password 
        |> Seq.countBy id 
        |> Map.ofSeq

    match charCounts |> Map.tryFind pwd.Char with
    | Some cnt -> cnt >= pwd.Min && cnt <= pwd.Max
    | None -> false

let isValid2 (pwd: Password) =
    let check idx = 
        idx > 0 
        && idx <= pwd.Password.Length 
        && pwd.Password.[idx-1] = pwd.Char
        
    match check pwd.Min, check pwd.Max with
    | true, false 
    | false, true -> true
    | _ -> false

let example =
    [| "1-3 a: abcde"
       "1-3 b: cdefg"
       "2-9 c: ccccccccc" |]

let solve validation input =
    input
    |> Array.choose parseLine
    |> Array.filter validation
    |> Array.length

solve isValid1 example
solve isValid2 example

solve isValid1 (File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input2.txt"))
solve isValid2 (File.ReadAllLines (__SOURCE_DIRECTORY__ + "/input2.txt"))