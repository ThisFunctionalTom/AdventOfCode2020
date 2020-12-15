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
// let factorise n =
//     let rec f n x a = 
//         if x = n then
//             x::a
//         elif n % x = 0UL then 
//             f (n/x) x (x::a)
//         else
//             f n (x+1UL) a
//     f n 2UL []

// let busses = readInput2 "Day13.input"
// busses 
// |> Array.map (fun (idx, busid) -> 
//     factorise busid
//     |> List.countBy id)

let solve2 (printEvery: int) (busses: (uint64*uint64) []) =
    let (maxOffset, maxBid) = 
        busses 
        |> Array.maxBy (fun (x, y) -> if x < uint64 busses.Length then x*y else y)

    let rec loop idx =
        let upper = idx * maxOffset * maxBid
        let ts = upper - maxOffset
        if idx%(uint64 printEvery) = 0UL then printfn "TS: %d, Upper: %d" ts upper
        let found =
            busses
            |> Array.forall (fun (iex, bid) -> (ts + idx)%bid = 0UL )
        // let found = busses |> Array.forall (fun (idx, bid) -> 
        //     let possibleSolutions = 
        //         Seq.initInfinite (fun i -> ts + (uint64 i)*bid)
        //         |> Seq.takeWhile (fun bts -> bts < upper)
        //         |> List.ofSeq
        //     List.exists (fun bts -> (bts + idx)%bid = 0UL) possibleSolutions)
        if found 
        then Some ts
        else loop (idx+1UL)

    loop 1UL

let show (solution: uint64) (busses: (uint64*uint64) []) =
    for (idx, bid) in busses do
        printfn "Idx: %d, BusId: %d - %d|%d" idx bid (solution/bid) (solution%bid)

readInput2 "example.input"
readInput2 "example.input" |> solve2 100

getBusses "17,x,13,19" |> solve2 10
getBusses "67,7,59,61" |> show 754018UL

59UL*12780UL
//|> solve2 10



let input = readInput2 "Day13.input"

for (idx, bid) in input do
    let x = input |> Array.filter (fun (i, b) -> i % bid = 0UL && i <> 0UL)
    if not (Array.isEmpty x) then
        printfn "Idx: %d, Bid: %d - %A" idx bid x

523*19