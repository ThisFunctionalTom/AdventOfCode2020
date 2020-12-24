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
open System.Reflection.Metadata.Ecma335
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

let isMatch (rules: Map<int, Match>) input ruleId =
    let showRule = function
        | Simple c -> $"\"{c}\""
        | Complex rules ->
            rules
            |> Array.map (fun seqRules -> seqRules |> Array.map string |> String.concat " ")
            |> String.concat " | "
    let rec ruleMatches ruleId (input: string) ident : string list =
        let rec matchSeq rulesSeq (input: string) =
            match rulesSeq with
            | [] -> [ input ]
            | nextRule::rest ->
                ruleMatches nextRule input (ident+"|  ")
                |> List.collect (fun oneInput -> matchSeq rest oneInput)

        let rule =
            match ruleId with
            | 8 -> Complex [| [| 42 |]; [| 42; 8 |]  |]
            | 11 -> Complex [| [| 42; 11; 31 |]; [| 42; 31 |] |]
            | _ -> rules.[ruleId]
//        printfn $"{ident}{ruleId}: %A{showRule rule} on <{input}>"
        let result =
            match rule with
            | Simple c ->
                if input.StartsWith(c)
                then [ input.[c.Length..] ]
                else []
            | Complex orRules ->
                orRules
                |> List.ofArray
                |> List.collect (fun rulesSeq -> matchSeq (List.ofArray rulesSeq) input)
        let success =
            match result with
            | [] -> "failed"
            | rest -> $"success: {rest}"
//        printfn $"{ident}{ruleId}: {success}"
        result
    ruleMatches ruleId input ""
    |> List.exists ((=) "")

let rules, input = readInput "example2.input"

let solve2 fileName =
    let rules, inputs = readInput fileName

    inputs
    |> Array.filter (fun input -> isMatch rules input 0 )

solve2 "example2.input" |> Array.length
solve2 "Day19.input" |> Array.length