open System.IO

let fileName = "Day04/day04.data"
let lines = File.ReadAllLines(fileName)

let parseSections (s: string) =
    match s.Split('-') with
    | [| fromSection ; toSection|] -> (int fromSection, int toSection)
    | _ -> failwithf "Section in invalid format: %s" s

let data =
    lines
    |> Array.map (fun line ->
        match line.Split(',') with
        | [| a ; b|] -> parseSections a, parseSections b
        | _ -> failwithf "Line in invalid format: %s" line)

let result =
    data
    |> Array.filter (fun ((aFrom,aTo),(bFrom,bTo)) ->
                        (aFrom >= bFrom && aFrom <= bTo) ||
                        (aTo >= bFrom && aTo <= bTo) ||
                        (bFrom >= aFrom && bFrom <= aTo) ||
                        (bTo >= aFrom && bTo <= aTo)
                    )
    |> Array.length

printfn "Result: %i" result