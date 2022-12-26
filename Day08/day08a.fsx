open System.IO

let fileName = "Day08/day08.data"
let lines = File.ReadAllLines(fileName)

let rowLength = Array.length lines
let colLength = lines[0].Length

let mapData =
    Array2D.init rowLength colLength (fun r c -> int <| lines[r][c..c])

let isVisible r c mapData =
    let rMin = 0
    let rMax = (Array2D.length1 mapData) - 1 
    let cMin = 0
    let cMax = (Array2D.length2 mapData) - 1
    let myHeight = mapData[r,c]

    if r = rMin || r = rMax || c = cMin || c = cMax then
        true
    else
        let left = seq { cMin..(c-1) } |> Seq.map (fun col -> mapData[r,col])
        let right = seq { (c+1)..cMax } |> Seq.map (fun col -> mapData[r,col])
        let up = seq { rMin..(r-1) } |> Seq.map (fun row -> mapData[row,c])
        let down = seq { (r+1)..rMax } |> Seq.map (fun row -> mapData[row,c])
        
        left |> Seq.forall (fun height -> height < myHeight) ||
        right |> Seq.forall (fun height -> height < myHeight) ||
        up |> Seq.forall (fun height -> height < myHeight) ||
        down |> Seq.forall (fun height -> height < myHeight)

let fold2D folder state arr2D =
    seq {
        for row in 0..(Array2D.length1 arr2D)-1 do
            for col in 0..(Array2D.length2 arr2D)-1 do
                yield row,col
    } |> Seq.fold folder state

let result =
    mapData
    |> fold2D (fun total (r,c) -> if isVisible r c mapData then total+1 else total) 0

printfn "Result: %i" result
