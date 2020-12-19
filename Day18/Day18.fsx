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
    if str.Length > 0 && str.[0] = c then Some str.[1..] else None

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