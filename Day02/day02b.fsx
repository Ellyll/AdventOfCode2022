open System.IO

type Choice =
    | Rock
    | Paper
    | Scissors

type RoundResult =
    | Win
    | Draw
    | Lose

let fileName = "Day02/day02.data"
let lines = File.ReadAllLines(fileName)

let getResultNeeded resultNeeded against =
    match resultNeeded with
    | Win -> match against with
             | Rock -> Paper
             | Paper -> Scissors
             | Scissors -> Rock
    | Draw -> against
    | Lose -> match against with
              | Rock -> Scissors
              | Paper -> Rock
              | Scissors -> Paper


let rounds =
    lines
    |> Array.map(fun line ->
            let player =
                match line[0] with
                | 'A' -> Rock
                | 'B' -> Paper
                | 'C' -> Scissors
                | c -> failwithf "Invalid character for player: %c" c
            let me =
                match line[2] with
                | 'X' -> getResultNeeded Lose player
                | 'Y' -> getResultNeeded Draw player
                | 'Z' -> getResultNeeded Win player
                | c -> failwithf "Invalid character for me: %c" c
            (player,me)
        )

let result =
    rounds
    |> Array.sumBy (fun (player, me) ->
                    let choiceScore =
                        match me with
                        | Rock -> 1
                        | Paper -> 2
                        | Scissors -> 3
                    let winnerScore =
                        match player, me with
                        | Rock, Rock -> 3
                        | Rock, Paper -> 6
                        | Rock, Scissors -> 0
                        | Paper, Rock -> 0
                        | Paper, Paper -> 3
                        | Paper, Scissors -> 6
                        | Scissors, Rock -> 6
                        | Scissors, Paper -> 0
                        | Scissors, Scissors -> 3
                    choiceScore + winnerScore
                    )

printfn "Result: %i" result