open System.IO

let parse (input: string) =
    input.Split(",") 
    |> Array.indexed
    |> Array.choose (function | (_, "x") -> None | (idx, str)  -> Some (uint64 idx, uint64 str))

let solve (busses: (uint64*uint64)[]) =
    let rec loop (start, step) busses =
        match busses with
        | [] -> start
        | (busOffset, bus)::rest ->
            let [| nextStart; nextStartAndStep |] =
                Seq.initInfinite (fun i -> uint64 i * step + start)
                |> Seq.filter (fun z -> (z + busOffset) % bus = 0UL)
                |> Seq.take 2
                |> Array.ofSeq
            let nextStep = nextStartAndStep - nextStart
            loop (nextStart, nextStep) rest

    loop (busses.[0]) (busses |> List.ofArray)
    |> sprintf "%d"

solve (parse "17,x,13,19") // 3417
solve (parse "67,7,59,61") // 754018
solve (parse "67,x,7,59,61") // 779210 
solve (parse "67,7,x,59,61") // 1261476 
solve (parse "1789,37,47,1889") // 1202161486 

parse "19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,523,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,x,x,x,x,x,x,x,x,x,x,29,x,853,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23"
|> solve // 210612924879242
