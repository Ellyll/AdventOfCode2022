open System.IO

let fileName = "Day06/day06.data"
let data = File.ReadAllText(fileName).TrimEnd()

let findStart (str: string) : int =
    if String.length str < 4 then
        failwithf "String too short: %s" str
    
    let rec loop idx =
        if idx >= String.length str then
            failwith "Start not found"
        let marker = str[idx-3..idx]
        if (Set.ofSeq marker) |> Seq.length = 4 then
            idx
        else
            loop (idx+1)
    loop 3

let result = 1 + findStart data

printfn "Result: %i" result