open System

let readInput (input: string) =
    input |> Array.ofSeq |> Array.map (string >> int)

let rec getDest (min, max) dest (exclude: int list) =
    if dest < min then getDest (min, max) max exclude
    elif exclude |> List.contains dest then getDest (min, max) (dest - 1) exclude
    else dest

type Cups(init: int [], ?count) =
    let count = defaultArg count init.Length

    let cups =
        Array.init count (fun idx -> (idx + 1) % count + 1)

    let set cupNr next = cups.[cupNr - 1] <- next

    do
        init
        |> Array.pairwise
        |> Array.iter (fun (prev, next) -> set prev next)

    do
        if init.Length = count then
            set init.[^0] init.[0]
        else
            set init.[^0] (init.Length + 1)
            set count init.[0]

    member _.Count = count

    member _.CupsArray =
        cups |> Array.mapi (fun idx lbl -> idx + 1, lbl)

    member _.Next cupNr = cups.[cupNr - 1]

    member _.Next(cupNr: int, count: int) =
        let rec loop count curr =
            if count = 0 then curr else loop (count - 1) cups.[curr - 1]

        loop count cupNr

    member _.ToList(cupNr: int, count: int) =
        List.unfold
            (function
            | 0, _ -> None
            | count, curr ->
                let next = cups.[curr - 1]
                Some(next, (count - 1, next)))
            (count, cupNr)

    member _.ToList(start: int) =
        List.unfold
            (function
            | None -> Some(start, Some cups.[start - 1])
            | Some curr when curr <> start -> Some(curr, Some cups.[curr - 1])
            | _ -> None)
            None

    member this.Move(src: int, dst: int, count: int) =
        let blockStart = this.Next src
        let blockEnd = this.Next(src, count)
        let blockNext = this.Next blockEnd
        let dstNext = this.Next dst

        set dst blockStart
        set blockEnd dstNext
        set src blockNext

let play moveCount curr (cups: Cups) =
    let getDest = getDest (1, cups.Count)

    let showCups (currCup) =
        cups.ToList currCup
        |> List.map string
        |> String.concat " "

    let showPickup (pickup: int list) =
        pickup |> List.map string |> String.concat ", "

    let rec loop move (curr: int) =
        if move > moveCount then
            // printfn ""
            // printfn "-- final --"
            // printfn $"cups: {showCups curr}"
            ()
        else
            //if move % 100000 = 0 then printfn $"Move: {move}"
            // printfn ""
            // printfn $"-- move {move} --"
            // printfn $"cups: {showCups curr}"

            let pickup = cups.ToList(curr, 3)
            //printfn $"pick up: {showPickup pickup}"

            let dest = getDest (curr - 1) pickup
            //printfn $"destination: {dest}"
            cups.Move(curr, dest, 3)
            loop (move + 1) (cups.Next curr)

    loop 1 curr

let solve1 (input: string) moveCount =
    let input = readInput input
    let cups = Cups(input)

    play moveCount input.[0] cups

    cups.ToList 1
    |> List.skip 1
    |> List.map string
    |> String.concat ""

solve1 "389125467" 10
solve1 "389125467" 100
solve1 "614752839" 100

let solve2 (input: string) moveCount cupsCount =
    let input = readInput input
    let cups = Cups(input, cupsCount)

    play moveCount input.[0] cups

    let [ first; second ] = cups.ToList (1, 2)
    let result = (uint64 first)*(uint64 second)
    printfn $"%d{first} * %d{second} = %d{result}"
    result

solve2 "389125467" 10_000_000 1_000_000
solve2 "614752839" 10_000_000 1_000_000