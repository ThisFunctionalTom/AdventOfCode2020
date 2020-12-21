type Match =
| Simple of string
| Complex of int [][]

let parseRules (lines: string []) =
    lines
    |> Array.map (fun line -> 
        let [| key; valueStr |] = line.Split(": ")
        let matchExpr =
            if valueStr.StartsWith("\"") then
                Simple (valueStr.Trim('"'))
            else
                valueStr.Split(" | ")
                |> Array.map (fun term -> term.Split(" ") |> Array.map int)
                |> Complex
        int key, matchExpr)
    |> Map.ofArray    

open System.IO

let readInput fileName =
    let txt =
        Path.Combine (__SOURCE_DIRECTORY__, fileName)
        |> File.ReadAllText
    
    let [| rulesStr; input |] = txt.Split("\r\n\r\n")

    parseRules (rulesStr.Split("\r\n")), input.Split("\r\n")

let getRegex (rules: Map<int, Match>) ruleId =
    let mutable regexes : Map<int, string> = Map.empty
    
    let rec loop ruleId =
        match regexes |> Map.tryFind ruleId with
        | Some value -> value
        | None ->
            printfn $"Calc ruleId: {ruleId}"
            let value =
                match rules.[ruleId] with
                | Simple value -> value
                | Complex references -> 
                    let expr = 
                        references
                        |> Array.map (fun refs ->
                            refs
                            |> Array.map loop
                            |> String.concat "")
                        |> String.concat "|"
                    $"({expr})"
            regexes <- regexes |> Map.add ruleId value
            value
    $"^{loop ruleId}$"

let solve fileName =
    let rules, input = readInput fileName
    let rule0 = getRegex rules 0
    printfn $"Rule0: {rule0}"
    input
    |> Array.filter (fun line -> System.Text.RegularExpressions.Regex.IsMatch(line, rule0) )
    |> Array.length

solve "example.input"
solve "Day19.input"

