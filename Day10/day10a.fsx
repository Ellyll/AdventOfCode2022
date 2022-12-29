open System.IO
open System.Text.RegularExpressions

let fileName = "Day10/day10.data"
let lines = File.ReadAllLines(fileName)

type Instruction =
    | Noop
    | AddX of V: int

let program =
    let reg = Regex(@"^(noop)|(addx -?\d+)$")
    lines
    |> Array.map (fun line ->
        if not <| reg.IsMatch line then
            failwithf "Invalid instruction: %s" line
        match line.Split(' ') with
        | [| "addx" ; v |] -> AddX <| int v
        | [| "noop" |] -> Noop
        | _ -> failwithf "Invalid instruction: %s" line
        )
    |> Array.toList

let runProgram (program: Instruction list) =
    let getDuration = function
        | Noop -> 0
        | AddX _ -> 1
    
    let signalChecks = [ 20 ; 60 ; 100 ; 140 ; 180 ; 220 ] |> Set.ofList

    let rec loop x currentInstruction remainingCycles instructions cycleNumber signalStrengths =
        match currentInstruction with
        | None ->
            if List.isEmpty instructions then
                // No current instruction and no instructions - nothing to do!
                signalStrengths
            else
                // No current instruction, so load the next one in
                let currInst = instructions |> List.head
                let currentInstruction' = Some currInst
                let instructions' = instructions |> List.tail
                let remainingCycles' = getDuration currInst
                loop x currentInstruction' remainingCycles' instructions' cycleNumber signalStrengths
        | Some instr ->
            let signalStrenghts' =
                if signalChecks |> Set.contains cycleNumber then
                    printfn "Cycle: %i X: %i Signal Strength: %i" cycleNumber x (cycleNumber*x)
                    (cycleNumber * x)::signalStrengths
                else
                    signalStrengths
            if remainingCycles > 0 then
                loop x currentInstruction (remainingCycles-1) instructions (cycleNumber+1) signalStrenghts'
            else
                match instr with
                | Noop -> loop x None 0 instructions (cycleNumber+1) signalStrenghts'
                | AddX v -> loop (x+v) None 0 instructions (cycleNumber+1) signalStrenghts'
    loop 1 None 0 program 1 []

let signalStrengths = runProgram program

let result = signalStrengths |> Seq.sum
printfn "Result: %i" result
