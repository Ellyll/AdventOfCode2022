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

let moveTail headPosition tailPosition =
    let rH,cH = headPosition
    let rT,cT = tailPosition
    let rDelta,cDelta = rH-rT,cH-cT
    // if touching no need to move
    if (abs rDelta) <= 1 && (abs cDelta) <= 1 then
        tailPosition
    else
        rT + sign rDelta, cT + sign cDelta

let _,_,visitedByTail =
    commands
    |> Array.fold (fun (hPosition,tPosition,visited) (direction,distance) ->
            seq { 1..distance }
            |> Seq.fold (fun (hPos,tPos,vs) _ -> 
                    let newHPos = hPos |> moveStep direction
                    let newTPos = tPos |> moveTail newHPos
                    newHPos,newTPos,(vs |> Set.add newTPos)
                ) (hPosition,tPosition,visited)
        ) ((0,0),(0,0),Set.empty)

let result = visitedByTail |> Set.count
printfn "Result: %i" result

