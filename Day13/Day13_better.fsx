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
            let start = 0UL
            let busOffset, bus = busses.[1]
            let step = 19UL
            let [| nextStart; nextStartAndStep |] =
                Seq.initInfinite (fun i -> uint64 i * step + start)
                |> Seq.filter (fun z -> (z - busOffset) % bus = 0UL)
                |> Seq.take 2
                |> Array.ofSeq
            let nextStep = nextStartAndStep - nextStart
            loop nextStart nextStep rest

    loop 0UL (busses.[0] busses

let busses = parse "19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,523,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,17,13,x,x,x,x,x,x,x,x,x,x,29,x,853,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23"