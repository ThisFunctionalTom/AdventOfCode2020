open System
open System.IO
open System.Text.RegularExpressions

let path fileName =
    Path.Combine(__SOURCE_DIRECTORY__, fileName)

module Array =
    let rec splitWhen p arr =
        let rec loop acc rest =
            [| match rest with
               | [] -> List.rev acc |> Array.ofList
               | x :: xs ->
                   if p x then
                       List.rev acc |> Array.ofList
                       yield! loop [] xs
                   else
                       yield! loop (x :: acc) xs |]

        loop [] (List.ofArray arr)

let parseLine (line: string) =
    line.Split ' '
    |> Array.map (fun item ->
        let [| k; v |] = item.Split ':'
        k, v)

let tryParseInt (str: string) =
    match Int32.TryParse str with
    | true, value -> Some value
    | _ -> None

let hasLength length (str: string) = 
    str.Length = length

let isInRange min max value =
    value >= min && value <= max

let isIntInRange min max (str: string) =
    match tryParseInt str with
    | None -> false
    | Some value -> isInRange min max value

let parseValueUnit (unit: string) (str: string) =
    if str.EndsWith unit then
        str.Substring(0, str.Length - unit.Length)
        |> tryParseInt
    else
        None

let (|Cm|Inch|Error|) (str: string) =
    let cm = parseValueUnit "cm" str
    let inch = parseValueUnit "in" str
    match cm, inch with
    | Some x, _ -> Cm x
    | _, Some x -> Inch x
    | _ -> Error

let isValidHeight str =
    match str with
    | Cm value -> isInRange 150 193 value
    | Inch value -> isInRange 59 76 value
    | Error -> false

let isValidRgbColor (str: string) =
    let re = Regex("#[0-9a-f]{6}")
    (re.Match str).Success

let isValidPassportId (str: string) =
    hasLength 9 str && str |> Seq.forall Char.IsDigit

let isOneOf lst str =
    lst |> List.contains str 

let eyeColors = [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]

let (<&>) f g value =
    f value && g value

let items =
    [ "byr", "(Birth Year)", hasLength 4 <&> isIntInRange 1920 2002
      "iyr", "(Issue Year)", hasLength 4 <&> isIntInRange 2010 2020
      "eyr", "(Expiration Year)", hasLength 4 <&> isIntInRange 2020 2030
      "hgt", "(Height)", isValidHeight
      "hcl", "(Hair Color)", isValidRgbColor
      "ecl", "(Eye Color)", isOneOf eyeColors
      "pid", "(Passport ID)", isValidPassportId ]
//      "cid", "(Country ID)" ]

let parseInput input =
    input
    |> Array.splitWhen ((=) "")
    |> Array.map (fun block -> block |> Array.collect parseLine)

let solve1 input =
    parseInput input
    |> Array.filter (fun kvs ->
        items
        |> List.forall (fun (r, _, _) -> kvs |> Array.map fst |> Array.contains r))
    |> Array.length

let solve2 input =
    parseInput input
    |> Array.filter (fun kvs ->
        items
        |> List.forall (fun (required, _, validation) -> 
            kvs 
            |> Array.tryFind (fun (key, _) -> key = required)
            |> Option.map (fun (key, value) -> validation value)
            |> Option.defaultValue false) )
    |> Array.length

let example = File.ReadAllLines(path "example.input")
let input = File.ReadAllLines(path "day4.input")

solve1 example
solve1 input

let invalid = File.ReadAllLines(path "invalid.input")
let valid = File.ReadAllLines(path "valid.input")

solve2 invalid
solve2 valid

solve2 input