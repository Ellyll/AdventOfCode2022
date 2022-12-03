open System.IO

let fileName = "Day01/day01.data"
let lines = File.ReadAllLines(fileName)

let (elves, _) =
    lines
    |> Array.fold (fun (elfMap,elfNumber) line ->
                    if (System.String.IsNullOrWhiteSpace(line)) then
                        elfMap, (elfNumber + 1)
                    else
                        elfMap |> Map.change elfNumber (fun vOption ->
                                                                let n = (System.Int32.Parse(line))
                                                                match vOption with
                                                                | Some v -> Some (v + n)
                                                                | None -> Some n
                                                              )
                        , elfNumber

                   ) (Map.empty,1)

let result =
    elves
    |> Map.toSeq
    |> Seq.sortByDescending (snd)
    |> Seq.take 3
    |> Seq.sumBy (snd)

printfn "Result: %i" result