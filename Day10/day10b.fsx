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

let crtHeight = 6
let crtWidth = 40

let getCrtCoords cycleNumber =
    let pixels = ((cycleNumber-1) % (crtHeight*crtWidth))
    let row = pixels / crtWidth
    let col = pixels % crtWidth
    row,col

let runProgram (program: Instruction list) =
    let getDuration = function
        | Noop -> 0
        | AddX _ -> 1
   
    let rec loop x currentInstruction remainingCycles instructions cycleNumber (crt: bool[,]) =
        match currentInstruction with
        | None ->
            if List.isEmpty instructions then
                // No current instruction and no instructions - nothing to do!
                crt
            else
                // No current instruction, so load the next one in
                let currInst = instructions |> List.head
                let currentInstruction' = Some currInst
                let instructions' = instructions |> List.tail
                let remainingCycles' = getDuration currInst
                loop x currentInstruction' remainingCycles' instructions' cycleNumber crt
        | Some instr ->
            // Check if pixel is visible and update array if it is
            let crtR,crtC = getCrtCoords cycleNumber
            let spriteCs = [ (x-1)..(x+1) ]
            if spriteCs |> List.contains crtC then
                crt.[crtR,crtC] <- true

            if remainingCycles > 0 then
                loop x currentInstruction (remainingCycles-1) instructions (cycleNumber+1) crt
            else
                match instr with
                | Noop -> loop x None 0 instructions (cycleNumber+1) crt
                | AddX v -> loop (x+v) None 0 instructions (cycleNumber+1) crt
    loop 1 None 0 program 1 (Array2D.create crtHeight crtWidth false)

let printCrt (crt: bool[,]) =
    for row in 0..(Array2D.length1 crt - 1) do
        for col in 0..(Array2D.length2 crt - 1) do
            if crt[row,col] then
                printf "#"
            else
                printf "."
        printfn ""

let crt = runProgram program

printfn "Result:"
printCrt crt
