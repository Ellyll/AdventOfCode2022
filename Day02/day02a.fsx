open System.IO

type Choice =
    | Rock
    | Paper
    | Scissors

let fileName = "Day02/day02.data"
let lines = File.ReadAllLines(fileName)

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
                | 'X' -> Rock
                | 'Y' -> Paper
                | 'Z' -> Scissors
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