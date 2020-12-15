open System
open System.IO

type Value = char[]

type Command =
| Mask of Value
| Write of {| Idx: Value; Value: Value |}

let toBinary value : Value =
    [| for i in [35..-1..0] do
        match value >>> i &&& 1UL with
        | 1UL -> '1'
        | _ -> '0' |]

let toDecimal (chars: Value) = 
    Convert.ToUInt64(String(chars), fromBase = 2)

let parseLine (line: string) =
    match line.Split([| " = "; "["; "]" |], StringSplitOptions.RemoveEmptyEntries) with
    | [| "mask"; mask |] -> Mask (Array.ofSeq mask)
    | [| "mem"; idx; value |] -> Write {| Idx = toBinary(uint64 idx); Value = toBinary (uint64 value) |}
    | x -> failwithf "not matched %s" line

let readInput fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines
    |> Array.map parseLine
    |> List.ofArray

let applyMask (mask: Value) (value: Value) : Value =
    Array.zip mask value
    |> Array.map (function | ('X', v) -> v | (m, _) -> m)

let solve input =
    let rec loop mask memory cmds =
        match cmds with
        | [] -> 
            memory 
            |> Map.fold (fun s _ v -> s + toDecimal v) 0UL
        | Mask msk :: rest -> 
            loop msk memory rest
        | Write write :: rest -> 
            let newValue = applyMask mask write.Value
            let newMemory = Map.add write.Idx newValue memory
            loop mask newMemory rest
    loop (toBinary 0UL) Map.empty input

solve (readInput "example.input")
solve (readInput "Day14.input")

let expandMask mask : Value list =
    let rec loop (acc: char list) bits =
        [ match bits with
          | [] -> acc
          | 'X' :: rest ->
            yield! loop ('0'::acc) rest
            yield! loop ('1'::acc) rest
          | c :: rest ->
            yield! loop ('1'::acc) rest ]
    loop [] (List.ofArray mask)
    |> List.map Array.ofList

let solve2 input =
    let rec loop masks memory cmds =
        match cmds with
        | [] -> 
            memory 
            |> Map.fold (fun s _ v -> s + toDecimal v) 0UL
        | Mask msk :: rest -> 
            loop (expandMask msk) memory rest
        | Write write :: rest -> 
            let newMemory =
                masks
                |> List.fold (fun m msk -> Map.add (applyMask msk write.Idx) write.Value m) memory
            loop masks newMemory rest
    loop List.empty Map.empty input

solve2 (readInput "example2.input")
solve2 (readInput "Day14.input")