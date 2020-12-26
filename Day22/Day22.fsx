open System
open System.IO

let readInput fileName =
    let lines =
        Path.Combine(__SOURCE_DIRECTORY__, fileName)
        |> File.ReadAllLines
    
    let firstDeck =
        lines.[1..]
        |> Array.takeWhile (not << String.IsNullOrEmpty)
        |> Array.map int
    
    let secondDeck =
        lines
        |> Array.skipWhile (not << String.IsNullOrEmpty)
        |> Array.skip 2
        |> Array.map int
    
    firstDeck, secondDeck

let rec playGame (p1: int[]) (p2: int[]) =
    match p1, p2 with
    | [||], _ -> p2
    | _, [||] -> p1
    | _, _ ->
        if p1.[0] > p2.[0] 
        then playGame (Array.append p1.[1..] [| p1.[0]; p2.[0] |]) p2.[1..]
        else playGame p1.[1..] (Array.append p2.[1..] [| p2.[0]; p1.[0] |])

let getResult deck =
    deck
    |> Array.rev
    |> Array.mapi (fun idx card -> (idx+1) * card)
    |> Array.sum

let solve fileName =
    let p1, p2 = readInput fileName
    playGame p1 p2
    |> getResult

solve "example.input"
solve "Day22.input"

type Player = Player1 | Player2

let playRecursiveCombatGame (p1: int[]) (p2: int[]) =
    let showDeck arr = arr |> Array.map string |> String.concat ", "
    let mutable gameId = 0
    let nextGameId () =
        gameId <- gameId + 1
        gameId
    let mutable playedSubGames = Map.empty
    let mutable cache = 0

    let rec loop game round (p1: int[]) (p2: int[]) (played: Set<int[]*int[]>) : Player*int[] =
        if round % 100000 = 0 then printfn $"-- Round {round} (Game {game}) -- stalled"
        match p1, p2 with
        | [||], _ -> Player2, p2
        | _, [||] -> Player1, p1
        | _, _ ->
            // printfn ""
            // printfn $"-- Round {round} (Game {game}) --"
            // printfn $"Player 1's deck: {showDeck p1}"
            // printfn $"Player 2's deck: {showDeck p2}"
            // printfn $"Player 1 plays: {p1.[0]}" 
            // printfn $"Player 2 plays: {p2.[0]}" 
            let winner =
                if Set.contains (p1, p2) played 
                then Player1
                elif p1.[0] < p1.Length && p2.[0] < p2.Length
                then 
                    let subGameId = nextGameId()
                    let p1Sub = p1.[1..p1.[0]] 
                    let p2Sub = p2.[1..p2.[0]]

                    match Map.tryFind (p1Sub, p2Sub) playedSubGames with
                    | Some (subGameWinner, game) -> 
                        cache <- cache + 1
                        subGameWinner
                    | None ->
                        // printfn $"Playing a sub-game to determine the winner..."
                        // printfn ""
                        // printfn $"=== Game {subGameId} ==="
                        let subGameWinner, _ = loop subGameId 1 p1Sub p2Sub Set.empty
                        // printfn $"The winner of game {subGameId} is {subGameWinner}!"
                        // printfn ""
                        // printfn $"...anyway, back to game {game}."
                        playedSubGames <- Map.add (p1Sub, p2Sub) (subGameWinner, subGameId) playedSubGames
                        if subGameId % 1000 = 0 
                        then printfn $"Played sub games: {subGameId} (Cache hits: {cache}, hit ratio: {float cache/float subGameId * 100.0}%%)"
                        subGameWinner
                elif p1.[0] > p2.[0]
                then Player1
                else Player2
            // printfn $"{winner} wins round {round} of game {game}!"
            let playedNew = Set.add (p1, p2) played
            match winner with
            | Player1 -> 
                let p1New = Array.append p1.[1..] [| p1.[0]; p2.[0] |]
                let p2New = p2.[1..]
                loop game (round+1) p1New p2New playedNew
            | Player2 -> 
                let p1New = p1.[1..]
                let p2New = Array.append p2.[1..] [| p2.[0]; p1.[0] |]
                loop game (round+1) p1New p2New playedNew

    // printfn $"=== Game 1 ==="
    let (winner, deck) = loop (nextGameId()) 1 p1 p2 Set.empty
    // printfn $"The winner of game 1 is {winner}!"
    winner, deck

let solve2 fileName =
    let p1, p2 = readInput fileName
    snd (playRecursiveCombatGame p1 p2)
    |> getResult

solve2 "example.input"
solve2 "Day22.input"