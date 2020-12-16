open System
open System.IO

type Range = {
    Min: int
    Max: int
} with 
    member range.Contains x = x >= range.Min && x <= range.Max

type Field = {
    Name: string
    Ranges: Range []
} with
    member field.IsValidValue value =
        field.Ranges
        |> Array.exists (fun r -> r.Contains value)

type Input = {
    Fields: Field []
    YourTicket: int []
    NearbyTickets: int [][] 
}

let readInput fileName =
    let lines =
        Path.Combine(__SOURCE_DIRECTORY__, fileName)
        |> File.ReadAllLines
    
    let parseTicket (line: string) = line.Split("," ) |> Array.map int

    let parseField (line: string) =
        let parseRange (str: string) =
            let [|min; max|] = str.Split("-") |> Array.map int
            { Min = min; Max = max }
        let parts = line.Split([|": "; " or "|], StringSplitOptions.None)
        let name = parts.[0]
        let ranges = parts.[1..] |> Array.map parseRange
        { Name = name; Ranges = ranges }

    let idx = lines |> Array.findIndex ((=) "")
    let fields = lines.[0..idx-1] |> Array.map parseField
    let yourTicket = lines.[idx+2] |> parseTicket
    let nearbyTickets = lines.[idx+5..] |> Array.map parseTicket

    { Fields = fields; YourTicket = yourTicket; NearbyTickets = nearbyTickets }

let solve (x: Input) =
    let allRanges =
        x.Fields
        |> Array.collect (fun field -> field.Ranges)

    x.NearbyTickets
    |> Array.collect (fun values ->
        values 
        |> Array.filter (fun value -> 
            allRanges
            |> Array.forall (fun range -> not (range.Contains value))))
    |> Array.sum

readInput "example.input" |> solve
readInput "Day16.input" |> solve

let getValidTickets (x: Input) =
    let allRanges =
        x.Fields
        |> Array.collect (fun f -> f.Ranges)

    x.NearbyTickets
    |> Array.filter (fun values ->
        values 
        |> Array.exists (fun value -> 
            allRanges
            |> Array.forall (fun range -> not (range.Contains value)))
        |> not)

let solve2 (x: Input) =
    let rec loop (possibleFields: Field[][]) input =
        match input with
        | [] -> possibleFields
        | ticket::tickets ->
            let possibleFields' =
                Array.zip possibleFields ticket
                |> Array.map (fun (possible, value) ->
                    possible
                    |> Array.filter (fun f -> f.IsValidValue value))
            loop possibleFields' tickets

    let validTickets = getValidTickets x
    let possible =
        loop (Array.replicate x.YourTicket.Length x.Fields) (List.ofArray validTickets)
        |> Array.indexed

    let rec loop2 (single: (int*Field)[]) (multi: (int*Field[])[]) =
        let newSingles, newMulti =
            multi 
            |> Array.partition (fun (_, pfs) -> pfs.Length = 1)
        
        let single = 
            newSingles 
            |> Array.map (fun (idx, fields) -> idx, fields.[0])
            |> Array.append single

        if Array.isEmpty newMulti 
        then single
        else
            newMulti
            |> Array.map (fun (idx, pfs) -> 
                idx, pfs |> Array.filter (fun f -> not (single |> Array.exists (fun (i, g) -> g = f))))
            |> loop2 single
    
    loop2 Array.empty possible
    |> Array.filter (fun (_, f) -> f.Name.StartsWith "departure")
    |> Array.map (fun (idx, f) -> idx, x.YourTicket.[idx])
    |> Array.fold (fun p (_, value) -> p * (uint64 value)) 1UL


readInput "example2.input" 
|> solve2

readInput "Day16.input" 
|> solve2
