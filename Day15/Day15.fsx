open System
open System.Diagnostics

let solve input maxIter =
    let timer = Stopwatch.StartNew()
    let rec loop maxIter iteration input last map =
        let next, newInput =
            match input with
            | [] ->
                match Map.find last map with
                | [ _ ] -> 0, []
                | [x; y] -> x - y, []
                | _ -> failwith "Ooops"
            | x::xs -> x, xs
        
        let newMap = 
            match Map.tryFind next map with
            | None -> Map.add next [iteration] map
            | Some lst -> Map.add next (iteration :: lst |> List.take 2) map

        if iteration % 1000000 = 0 then 
            let estimate = timer.Elapsed.TotalSeconds * (float (maxIter - iteration)) / float iteration
            printfn "Estimate: %A" (TimeSpan.FromSeconds estimate)

        if iteration = maxIter
        then next 
        else loop maxIter (iteration+1) newInput next newMap

    loop maxIter 1 input 0 Map.empty

solve [0; 3; 6] 2020
solve [ 18; 8; 0; 5; 4; 1;20 ] 2020

solve [ 18; 8; 0; 5; 4; 1;20 ] 30000000