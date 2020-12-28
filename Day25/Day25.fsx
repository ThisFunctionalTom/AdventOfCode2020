let values (subject: uint64) (start: uint64) =
    Seq.unfold (fun v -> Some (v, v * subject % 20201227UL) ) start

let solve1 subject cardPk doorPk =
    let doorLoop =
        values subject 1UL
        |> Seq.findIndex ((=) doorPk)
    let cardLoop =
        values subject 1UL
        |> Seq.findIndex ((=) cardPk)
    
    let cardEk =
        values doorPk 1UL
        |> Seq.item cardLoop

    let doorEk =    
        values cardPk 1UL
        |> Seq.item doorLoop
    cardEk, doorEk

solve1 7UL 5764801UL 17807724UL
solve1 7UL 15733400UL 6408062UL
