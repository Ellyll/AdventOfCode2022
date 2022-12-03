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
    |> Array.map (fun line ->
        let len = line |> String.length
        let container1 = line[0..((len/2)-1)]
        let container2 = line[(len/2)..]
        container1,container2
        )

let result =
    rucksacks
    |> Array.sumBy(fun (container1, container2) ->
        let a = container1 |> Set.ofSeq
        let b = container2 |> Set.ofSeq
        let inBoth = Set.intersect a b
        inBoth
        |> Seq.sumBy (getPriority)
        )

printfn "Result: %i" result
