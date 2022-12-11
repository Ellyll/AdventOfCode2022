open System.IO

let fileName = "Day06/day06.data"
let data = File.ReadAllText(fileName).TrimEnd()

let findStart (str: string) : int =
    let messageLength = 14

    if String.length str < messageLength then
        failwithf "String too short: %s" str
    
    let rec loop idx =
        if idx >= String.length str then
            failwith "Start not found"
        let marker = str[idx-(messageLength-1)..idx]
        if (Set.ofSeq marker) |> Seq.length = messageLength then
            idx
        else
            loop (idx+1)
    loop (messageLength-1)

let result = 1 + findStart data

printfn "Result: %i" result