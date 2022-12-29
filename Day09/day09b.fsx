open System.IO
open System.Text.RegularExpressions

let fileName = "Day09/day09.data"
let lines = File.ReadAllLines(fileName)

type Direction =
    | Right
    | Left
    | Up
    | Down

let commands =
    let reg = Regex(@"^(L|R|U|D) (\d+)$")
    lines
    |> Array.map (fun line ->
        if not (reg.IsMatch(line)) then
            failwithf "Invalid command: %s" line
        match line.Split(' ') with
            | [| "L" ; s |] -> Left, s |> int
            | [| "R" ; s |] -> Right, s |> int
            | [| "U" ; s |] -> Up, s |> int
            | [| "D" ; s |] -> Down, s |> int
            | _ -> failwithf "Invalid command: %s" line
        )

let moveStep direction (row,column) =
    match direction with
    | Left -> row, (column-1)
    | Right -> row, (column+1)
    | Up -> (row+1), column
    | Down -> (row-1), column

let moveKnot headPosition knotPosition =
    let rH,cH = headPosition
    let rK,cK = knotPosition
    let rDelta,cDelta = rH-rK,cH-cK
    // if touching no need to move
    if (abs rDelta) <= 1 && (abs cDelta) <= 1 then
        knotPosition
    else
        rK + sign rDelta, cK + sign cDelta

let initialPositions =
    Array.create 10 (0,0)
    |> Array.toList

let initialVisited = Set.singleton (0,0)

let _,visitedByTail =
    commands
    |> Array.fold (fun (positions : (int*int) list, visited : Set<int*int>) (direction,distance) ->
            seq { 1..distance }
            |> Seq.fold (fun (ps : (int*int) list, vs : Set<int*int>) _ -> 
                    let newHeadPosition = ps |> List.head |> moveStep direction
                    let newRemaining = ps |> List.tail
                    let ps' =
                        newHeadPosition::
                        (newRemaining
                        |> List.fold (fun (prevPosition,newPs) currPosition ->
                                        let newPosition = currPosition |> moveKnot prevPosition
                                        (newPosition,(newPosition::newPs)))
                                    (newHeadPosition, [])
                        |> snd
                        |> List.rev)
                    let vs' = vs |> Set.add (ps' |> List.last)
                    (ps',vs')
                ) (positions,visited)
            ) (initialPositions,initialVisited)

let result = visitedByTail |> Set.count
printfn "Result: %i" result
