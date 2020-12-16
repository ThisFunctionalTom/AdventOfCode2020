open System.IO
let readInput fileName =
    let [| ts; bussesStr |] =
        Path.Combine(__SOURCE_DIRECTORY__, fileName)
        |> File.ReadAllLines
    
    let busses = 
        bussesStr.Split(',') 
        |> Array.filter (fun str -> str <> "x")
        |> Array.map uint64
        
    
    uint64 ts, busses

let solve1 fileName = 
    let ts, busses = readInput fileName

    busses
    |> Array.map (fun bid -> bid, (bid - ts%bid))
    |> Array.minBy snd
    |> (fun (bid, min) -> bid * min)

solve1 "example.input"
solve1 "Day13.input" |> printfn "Solution1: %d"

let getBusses (bussesStr: string) =
    bussesStr.Split(',') 
    |> Array.indexed
    |> Array.choose (fun (idx, str) ->
        if str = "x"
        then None
        else Some (uint64 idx, uint64 str)
    )

let readInput2 fileName =
    let [| _; bussesStr |] =
        Path.Combine(__SOURCE_DIRECTORY__, fileName)
        |> File.ReadAllLines
    
    getBusses bussesStr

let solve2 startTs (busses: (uint64*uint64) []) =
    let first = snd busses.[0]
    let (offset, maxBusId) = 
        busses 
        |> Array.map (fun (bidx, bid) -> if bidx > 0UL && bidx % first = 0UL then bidx, bid*first else bidx, bid)
        |> Array.maxBy snd

    let rec loop idx =
        let ts = maxBusId*idx - offset
        if idx % 100000000UL = 0UL then printfn "%A" ts
        if busses |> Array.forall (fun (bidx, bid) -> (ts + bidx)%bid = 0UL )
        then Some ts
        else loop (idx+1UL)

    let startIdx = startTs / maxBusId + 1UL
    printfn "Step: %d, StartIdx: %d" maxBusId startIdx
    loop startIdx

let parse (input: string) =
    input.Split(",") 
    |> Array.indexed
    |> Array.choose (function | (_, "x") -> None | (idx, str)  -> Some (uint64 idx, uint64 str))

parse "17,x,13,19" |> solve2 0UL // 3417
parse "67,7,59,61" |> solve2 0UL // 754018
parse "67,x,7,59,61" |> solve2 0UL // 779210
parse "67,7,x,59,61" |> solve2 0UL // 1261476
parse "1789,37,47,1889" |> solve2 0UL // 1202161486

parse "19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,523,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,x,x,x,x,x,x,x,x,x,x,29,x,853,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23" |> solve2 100000000000000UL