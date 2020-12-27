open System

let readInput (input: string) =
    input 
    |> Array.ofSeq 
    |> Array.map (string >> int)

type Cup(label: int) =
    let mutable (next: Cup option) = None
    let mutable (prev: Cup option) = None
    member _.Next 
        with get () = next |> Option.get
        and set n = next <- Some n
    member _.Prev 
        with get () = prev |> Option.get
        and set p = prev <- Some p
    member _.Label = label

module Cups =
    let rec findForward maxAttempts lbl (curr: Cup) =
        if curr.Label = lbl then curr
        elif maxAttempts = 0 
        then failwith $"Label {lbl} not found"
        else findForward (maxAttempts-1) lbl curr.Next

    let rec findBackward maxAttempts lbl (curr: Cup) =
        if curr.Label = lbl then curr
        elif maxAttempts = 0 
        then failwith $"Label {lbl} not found"
        else findForward (maxAttempts-1) lbl curr.Prev
    
    let move3 (src: Cup) (dst: Cup) =
        let pickup1 = src.Next
        let pickup3 = pickup1.Next.Next

        let srcNext = pickup3.Next
        let dstNext = dst.Next

        // Remove pickup from source
        src.Next <- srcNext
        srcNext.Prev <- src

        // Insert into dest
        pickup1.Prev <- dst
        pickup3.Next <- dstNext
        dstNext.Prev <- pickup3
        dst.Next <- pickup1
    
    let getNext3Labels (curr: Cup) =
        [ curr.Next.Label; curr.Next.Next.Label; curr.Next.Next.Next.Label ]

    let toList (start: Cup) =
        List.unfold (fun prev ->
            match prev with
            | None -> Some (start.Label, Some start.Next)
            | Some cup when cup.Label <> start.Label -> Some (cup.Label, Some cup.Next)
            | _ -> None) None

    let unfold f =
        let rec loop (first: Cup) (last: Cup) idx =
            match f idx with
            | Some lbl -> 
                let newCup = Cup(lbl, Prev = last)
                last.Next <- newCup
                loop first newCup (idx+1)
            | None ->
                first.Prev <- last
                last.Next <- first
                first
        
        match f 0 with
        | Some lbl -> 
            let start = Cup(lbl)
            loop start start 1
        | None -> failwith "Invlid cups constructor"

let rec getDest (min, max) dest (exclude: int list) =
    if dest < min
    then getDest (min, max) max exclude
    elif exclude |> List.contains dest
    then getDest (min, max) (dest-1) exclude
    else dest

let createCups cupsCount (input: int[])  =
    Cups.unfold (fun idx -> 
        if idx >= cupsCount 
        then None
        elif idx < input.Length
        then Some input.[idx]
        else Some (idx+1) )

let play cupsCount moveCount (input: int[]) =
    let getDest = getDest (1, cupsCount)

    let showCups (curr: Cup) =
        Cups.toList curr
        |> List.map string
        |> String.concat " "

    let showPickup (pickup: int list) =
        pickup |> List.map string |> String.concat ", "

    let rec loop move (currCup: Cup) =
        if move > moveCount then
            // printfn ""
            // printfn "-- final --"
            // printfn $"cups: {showCups currCup}"
            currCup
        else
            if move % 1000 = 0 then printfn $"Move: {move}"
            // printfn ""
            // printfn $"-- move {move} --"
            // printfn $"cups: {showCups currCup}"
            let pickup = Cups.getNext3Labels currCup
            // printfn $"pick up: {showPickup pickup}"
            
            let destLabel = getDest (currCup.Label - 1) pickup
            // printfn $"destination: {destLabel}"
            let destCup = Cups.findBackward cupsCount destLabel currCup
            Cups.move3 currCup destCup
            loop (move + 1) currCup.Next

    createCups cupsCount input 
    |> loop 1

let getResult1 cupsCount (cup: Cup) =
    let one = Cups.findForward cupsCount 1 cup
    Cups.toList one
    |> List.skip 1
    |> List.map string
    |> String.concat ""

let solve1 (input: string) =
    let cupsCount = input.Length
    readInput input
    |> play cupsCount 100
    |> getResult1 cupsCount

solve1 "614752839"

let getResult2 cupsCount (cup: Cup) =
    let one = Cups.findForward cupsCount 1 cup
    one.Next.Label * one.Next.Next.Label

let solve2 (input: string) =
    let cupsCount = 1_000_000
    readInput input
    |> play cupsCount 10_000_000
    |> getResult2 cupsCount

solve2 "389125467"