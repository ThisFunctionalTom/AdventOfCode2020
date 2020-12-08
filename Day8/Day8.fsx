open System.IO

let readAllLines fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)
    |> File.ReadAllLines

let solve1 (prog: string[]) =
    let rec loop history acc ip =
        let [|cmd; valStr|] = prog.[ip].Split(" ")
        let value = int valStr
        if Set.contains ip history 
        then acc
        else
            let history' = Set.add ip history 
            match cmd with
            | "nop" -> loop history' acc (ip+1)
            | "acc" -> loop history' (acc + value) (ip+1)
            | "jmp" -> loop history' acc (ip+value)
            | _ -> failwithf "Invalid command %s" prog.[ip]
    loop Set.empty 0 0

readAllLines "example.input"
|> solve1

readAllLines "Day8.input"
|> solve1

let solve2 (prog: string[]) =
    let rec tryGetAcc history acc ip =
        if ip = prog.Length 
        then Some acc
        else
            let [|cmd; valStr|] = prog.[ip].Split(" ")
            let value = int valStr
            if Set.contains ip history 
            then None
            else
                let history' = Set.add ip history 
                match cmd with
                | "nop" -> tryGetAcc history' acc (ip+1)
                | "acc" -> tryGetAcc history' (acc + value) (ip+1)
                | "jmp" -> tryGetAcc history' acc (ip+value)
                | _ -> failwithf "Invalid command %s" prog.[ip]

    let rec loop history acc ip =
        let [|cmd; valStr|] = prog.[ip].Split(" ")
        let value = int valStr
        let history' = Set.add ip history 
        // nop is like jmp +1
        let jmp x = tryGetAcc history' acc (ip + x)
        let tryJump jmp1 jmp2 = 
            let res1 = jmp jmp1
            if Option.isSome res1 then res1
            else loop history' acc (ip + jmp2)
        match cmd with
        | "nop" -> tryJump value 1
        | "jmp" -> tryJump 1 value
        | "acc" -> loop history' (acc + value) (ip+1)
        | _ -> failwithf "Invalid command %s" prog.[ip]
    loop Set.empty 0 0

readAllLines "example.input"
|> solve2

readAllLines "Day8.input"
|> solve2
