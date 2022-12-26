open System.IO

let fileName = "Day08/day08.data"
let lines = File.ReadAllLines(fileName)

let rowLength = Array.length lines
let colLength = lines[0].Length

let mapData =
    Array2D.init rowLength colLength (fun r c -> int <| lines[r][c..c])

let getScenicScore r c mapData =
    let rMin = 0
    let rMax = (Array2D.length1 mapData) - 1 
    let cMin = 0
    let cMax = (Array2D.length2 mapData) - 1
    let myHeight = mapData[r,c]

    let getViewingDistance heights =
        let rec loop hs total =
            match Seq.tryHead hs with
            | None -> total
            | Some height ->
                let total' = total + 1
                if height >= myHeight then
                    total'
                else
                    loop (Seq.tail hs) total'
        loop heights 0

    let left = if c = cMin then 0 else seq { (c-1)..(-1)..cMin } |> Seq.map (fun col -> mapData[r,col]) |> getViewingDistance
    let right = if c = cMax then 0 else seq { (c+1)..cMax } |> Seq.map (fun col -> mapData[r,col]) |> getViewingDistance
    let up = if r = rMin then 0 else seq { (r-1)..(-1)..rMin } |> Seq.map (fun row -> mapData[row,c]) |> getViewingDistance
    let down = if r = rMax then 0 else seq { (r+1)..rMax } |> Seq.map (fun row -> mapData[row,c]) |> getViewingDistance
    (left * right * up * down)

let fold2D folder state arr2D =
    seq {
        for row in 0..(Array2D.length1 arr2D)-1 do
            for col in 0..(Array2D.length2 arr2D)-1 do
                yield row,col
    } |> Seq.fold folder state

let result =
    mapData
    |> fold2D (fun scores (r,c) -> (getScenicScore r c mapData)::scores ) []
    |> List.max

printfn "Result: %i" result
