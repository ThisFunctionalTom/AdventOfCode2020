open System

let getDigits (str: string) =
    str
    |> Seq.takeWhile Char.IsDigit
    |> Array.ofSeq
    |> String

let (|Int64|_|) (str: string) =
    let digits = getDigits str
    if digits.Length > 0 
    then Some(int64 digits, str.[digits.Length..]) 
    else None

let (|Char|_|) c (str: string) =
    if str.Length > 0 && str.[0] = c 
    then Some str.[1..] 
    else None

let solve (line: string) =
    let rec evalOperand (str: string) =
        match str with
        | "" -> failwith "Missing operand"
        | Int64 (value, rest) -> value, rest
        | Char '(' rest ->
            let opValue, rest = evalExpr rest
            opValue, rest
        | _ -> failwithf "Invalid operand %s" str

    and evalExpr (str: string) =
        let x, rest = evalOperand str
        evalOperation x rest

    and evalOperation x (str: string) =
        match str with
        | "" -> x, ""
        | Char ')' rest -> x, rest
        | Char '+' rest ->
            let (y, rest) = evalOperand rest
            evalOperation (x + y) rest
        | Char '*' rest ->
            let (y, rest) = evalOperand rest
            evalOperation (x * y) rest
        | _ -> failwithf "Invalid expression %s" str

    evalExpr (line.Replace(" ", "")) |> fst

let examples =
    [ "1 + 2 * 3 + 4 * 5 + 6", 71
      "1 + (2 * 3) + (4 * (5 + 6))", 51
      "2 * 3 + (4 * 5)", 26
      "5 + (8 * 3 + 9 + 3 * 4 * 3)", 437
      "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240
      "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632 ]

examples
|> List.filter (fun (input, solution) -> solve input <> int64 solution)

open System.IO

Path.Combine(__SOURCE_DIRECTORY__, "Day18.input")
|> File.ReadAllLines
|> Array.sumBy solve

let eval (str: string) =
    let rec evalValue (str: string) =
        match str with
        | Int64 result -> result
        | Char '(' rest -> evalExpr rest
        | _ -> failwith $"Missing value {str}"
    and evalExpr (str: string) =
        let value, rest = evalValue str
        evalMultiplication value rest
    and evalMultiplication x str =
        match str with
        | "" -> x, ""
        | Char ')' rest -> 
            x, rest
        | Char '+' _rest -> 
            let x, rest = evalAddition x str
            evalMultiplication x rest
        | Char '*' rest ->
            let y, rest = evalValue rest
            let y, rest = evalAddition y rest
            evalMultiplication (x*y) rest
        | _ -> failwith $"Invalid multiplication: {str}"
    and evalAddition x (str: string) =
        match str with
        | "" -> x, ""
        | Char ')' rest -> x, str
        | Char '*' rest -> x, str
        | Char '+' rest ->
            let y, rest = evalValue rest
            evalAddition (x + y) rest
        | _ -> failwith $"Invalid addition: {str}"
    fst (evalExpr (str.Replace(" ", "")))

[
    "1 + (2 * 3) + (4 * (5 + 6))", 51
    "2 * 3 + (4 * 5)", 46
    "5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340
]
|> List.iter (fun (str, expected) -> 
    let actual = eval str
    if actual <> int64 expected then
        eval str |> ignore
        printfn $"{str}"
        printfn $"Expected: {expected}"
        printfn $"Actual: {actual}" )

Path.Combine(__SOURCE_DIRECTORY__, "Day18.input")
|> File.ReadAllLines
|> Array.sumBy eval
