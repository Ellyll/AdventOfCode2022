open System.IO

let fileName = "Day05/day05.data"
let lines = File.ReadAllLines(fileName)

type ReadState =
        | LoadingCrates
        | LoadingInstructions

type Instruction = { Amount: int ; From: int ; To : int }

let parseInstruction (str: string) =
    match str.Replace("move ","").Replace(" from ", ",").Replace(" to ", ",").Split(',') with
    | [| a ; b; c |] -> { Amount = int a ; From = int b ; To = int c }
    | _ -> failwithf "Invalid instruction format: %s" str

let (crates, instructions, _) =
    lines
    |> Array.fold (fun (crs,instrs,readState) line ->
                    match readState with
                    | LoadingCrates ->
                        if line = "" then
                            crs,instrs,LoadingInstructions
                        else
                            let crs' =
                                line
                                |> Seq.mapi (fun i (c: char) -> i,c)
                                |> Seq.fold (fun cr (i,c) ->
                                                if c >= 'A' && c <= 'Z' then
                                                    let createNumber = ((i-1)/4)+1
                                                    cr
                                                    |> Map.change createNumber (fun vOpt ->
                                                        match vOpt with
                                                        | None -> Some [c]
                                                        | Some xs -> Some (xs @ [c]))
                                                else
                                                    cr
                                            ) (crs: Map<int,char list>)
                            crs',instrs,readState
                    | LoadingInstructions ->
                        let instruction = parseInstruction line
                        crs,instrs @ [ instruction ],readState

    ) (Map.empty,[],LoadingCrates)

let rearranged =
    instructions
    |> List.fold (fun crs instr ->
                    let rec loop ins cs =
                        if ins.Amount <= 0 then
                            cs
                        else
                            let crateToMove = cs |> Map.find ins.From |> List.head
                            let cs' = cs
                                    // Remove crate from old stack
                                    |> Map.change ins.From (fun vOpt ->
                                        match vOpt with
                                        | None -> failwith "Attempt to remove create from empty stack"
                                        | Some xs -> Some (xs |> List.tail)
                                        )
                                    // Place crate on new stack
                                    |> Map.change ins.To (fun vOpt ->
                                        match vOpt with
                                        | None -> Some [crateToMove]
                                        | Some xs -> Some (crateToMove::xs)
                                        )
                            loop { ins with Amount = ins.Amount - 1 } cs'
                    loop instr crs
                    ) crates

let result =
    rearranged
    |> Map.map (fun k v -> v |> List.head)
    |> Map.values
    |> Seq.toArray
    |> fun cs -> new System.String(cs)

printfn "Result: %s" result