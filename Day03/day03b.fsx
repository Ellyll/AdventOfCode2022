open System.IO

let fileName = "Day03/day03.data"
let lines = File.ReadAllLines(fileName)

let getPriority (character : char) : int =
    let ci =  int character
    if ci >= 65 && ci <= 90 then // A-Z
        ci - 64 + 26
    elif ci >= 97 && ci <= 122 then // a-z
        ci - 96
    else
        failwithf "Invalid character: %c" character

let rucksacks =
    lines
    |> Array.map (Set.ofSeq)

let groups = rucksacks |> Array.chunkBySize 3

let result =
    groups
    |> Array.sumBy (fun group ->
        match group with
        | [| a ; b ; c |]
                -> (Set.intersect a b) |> Set.intersect c |> Seq.sumBy getPriority
        | _ -> failwithf "Invalid group: %A" group
        )

printfn "Result: %i" result
