open System.IO

module String =
    let normalizeNewLine (str: string) =
        str.Replace("\r\n", "\n")

    let lines (str: string) = 
        (normalizeNewLine str).Split("\n")

    let lineGroups (str: string) = 
        (normalizeNewLine str).Split("\n\n")
        |> Array.map lines

let readInput (file: string) =
    File.ReadAllText(__SOURCE_DIRECTORY__ + "/" + file)

let solve1 (input: string) =
    input
    |> String.lineGroups
    |> Array.sumBy (fun group -> group |> Array.collect Array.ofSeq |> Array.distinct |> Array.length)

readInput "example.input" |> solve1
readInput "Day6.input" |> solve1

let solveGroup (group: string[]) =
    let count = group.Length

    group
    |> Array.collect Array.ofSeq
    |> Array.countBy id
    |> Array.filter (fun (_, cnt) -> cnt = count)
    |> Array.length

let solve2 (input: string) =
    input
    |> String.lineGroups
    |> Array.sumBy solveGroup

readInput "example.input" |> solve2
readInput "Day6.input" |> solve2