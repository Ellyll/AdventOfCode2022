#r "nuget:FSharp.Quotations.Evaluator"
open System.IO
open FSharp.Quotations.Evaluator

let fileName = "Day11/day11.data"
let lines = File.ReadAllLines(fileName)

type Monkey = {
    Items: int list
    Operation: int->int
    Test: int
    TrueTarget: int
    FalseTarget: int
    NoOfInspections : int }

let makeOperationFun (str: string) =
    match str.Split(' ') with
    | [| left ; op ; right |] when op = "+" || op = "*" ->
        let opExpr = if op = "+" then <@ (+) @> else <@ (*) @>
        let opFn = QuotationEvaluator.Evaluate opExpr
        let expr =
            match left,right with
            | "old", "old" ->
                <@ fun old -> opFn old old @>
            | "old", _ ->
                let r = int right
                <@ fun old -> opFn old r @>
            | _ , "old" ->
                let l = int left
                <@ fun old -> opFn l old @>
            | _ , _ ->
                let r = int right
                let l = int left
                <@ fun old -> opFn l r @>
        QuotationEvaluator.Evaluate expr
    | _ -> failwithf "Invalid Operation expression: %s" str

let _, monkeys =
    lines
    |> Array.fold (fun (currentId, monkeys) line ->
                if System.String.IsNullOrWhiteSpace(line) then
                    currentId, monkeys
                elif line.StartsWith("Monkey ") then
                    let currentId' = int <| line.Replace("Monkey ", "").Replace(":", "")
                    currentId',monkeys                    
                else
                    let currentMonkey =
                        match monkeys |> Map.tryFind currentId with
                        | Some monkey -> monkey
                        | None -> { Items = [] ; Operation = id ; Test = 0 ; TrueTarget = 0 ; FalseTarget = 0 ; NoOfInspections = 0 }
                    let currentMonkey' =
                        if line.Trim().StartsWith("Starting items: ") then
                            let items =
                                line.Trim().Replace("Starting items: ", "").Split(',')
                                |> Array.map (fun item -> int <| item.Trim())
                                |> Array.toList
                            { currentMonkey with Items = items }
                        elif line.Trim().StartsWith("Operation: new = ") then
                            let operation = line.Trim().Replace("Operation: new = ", "")
                            { currentMonkey with Operation = makeOperationFun operation }
                        elif line.Trim().StartsWith("Test: divisible by ") then
                            let test = int <| line.Trim().Replace("Test: divisible by ", "")
                            { currentMonkey with Test = test }
                        elif line.Trim().StartsWith("If true: throw to monkey ") then
                            let trueTarget = int <| line.Trim().Replace("If true: throw to monkey ", "")
                            { currentMonkey with TrueTarget = trueTarget }
                        elif line.Trim().StartsWith("If false: throw to monkey ") then
                            let falseTarget = int <| line.Trim().Replace("If false: throw to monkey ", "")
                            { currentMonkey with FalseTarget = falseTarget }
                        else
                            failwithf "Invalid line: %s" line

                    let monkeys' =
                        monkeys |> Map.change currentId (fun _ -> Some currentMonkey')

                    currentId, monkeys'
        ) (0,Map.empty)


let takeTurn (monkeyId: int) (monkeys: Map<int,Monkey>) : Map<int,Monkey> =
    let monkey = monkeys[monkeyId]
    let items = monkey.Items
    let monkey' = { monkey with Items = [] ; NoOfInspections = monkey.NoOfInspections + List.length monkey.Items }
    let monkeys' =
        monkeys |> Map.change monkeyId (fun v ->
            match v with
            | None -> failwithf "Monkey not found: %i " monkeyId
            | Some _ -> Some monkey')

    items
    |> List.fold (fun ms item ->
            let worryLevel = (monkey.Operation item) / 3
            let target =
                if (worryLevel % monkey.Test = 0 ) then
                    monkey.TrueTarget
                else
                    monkey.FalseTarget
            ms |> Map.change target (fun v ->
                match v with
                | None -> failwithf "Monkey not found: %i for item %i for monkey %A" target item monkey
                | Some m ->
                    Some { m with Items = m.Items @ [ worryLevel ]})
        ) monkeys'

let runRound monkeys =
    monkeys
    |> Map.keys
    |> Seq.fold (fun ms monkeyId ->
        takeTurn monkeyId ms
        ) monkeys

let finalMonkeys =
    seq { 1..20 }
    |> Seq.fold (fun ms _ -> runRound ms) monkeys

let result =
    finalMonkeys
    |> Map.keys
    |> Seq.map (fun monkeyId -> finalMonkeys[monkeyId].NoOfInspections)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.toList
    |> function
        | [ a ; b ] -> a * b
        | xs -> failwithf "Invalid monkey order: %A" xs

printfn "Result: %i" result
