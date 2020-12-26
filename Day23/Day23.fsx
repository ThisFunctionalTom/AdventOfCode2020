let readInput (input: string) =
    input
    |> Array.ofSeq 
    |> Array.map (string >> int)

let pickup3 currIdx (cups: int[]) =
    let pickupIdxs =
        [| currIdx+1%cups.Length
           currIdx+2%cups.Length
           currIdx+3%cups.Length |]
    cups
    |> Array.indexed
    |> Array.partition (fun (idx, _) -> Array.contains idx pickupIdxs)
    |> (fun (pickup, rest) -> Array.map snd pickup, Array.map snd rest)

module Array =
    let rotate n arr =
        let left, right = arr |> Array.splitAt n
        Array.append right left

let rec getDest dest (cups: int[]) =
    if cups |> Array.exists ((=) dest)
    then dest
    elif dest-1 > 0
    then getDest (dest-1) cups
    else getDest (cups |> Array.max) cups

let play count (cups: int[]) =
    let showCups cups = 
        cups 
        |> Array.map string
        |> String.concat " "

    let showPickup (pickup: int[]) =
        pickup |> Array.map string |> String.concat ", "
    
    let rec loop move cups =
        if move > count
        then
            printfn ""
            printfn "-- final --" 
            printfn $"cups: {showCups cups}"
            cups
        else
            printfn ""
            printfn $"-- move {move} --" 
            printfn $"cups: {showCups cups}"
            let curr = cups.[0]
            let cups' = Array.rotate 1 cups
            let pickup, rest = Array.splitAt 3 cups'
            
            printfn $"pick up: {showPickup pickup}"

            let dest = getDest (curr-1) rest
            let destIdx = Array.findIndex ((=) dest) rest
            printfn $"destination: {dest}"
            let left, right = rest |> Array.splitAt (destIdx + 1)
            Array.concat [| left; pickup; right |]
            |> loop (move + 1)

    loop 1 cups

let getResult (cups: int[]) =
    let idx = Array.findIndex ((=) 1) cups
    let left, right = Array.splitAt idx cups
    Array.append right.[1..] left
    |> Array.map string
    |> String.concat ""

readInput "614752839"
|> play 100
|> getResult