open System.IO

let fileName = "Day12/day12.test.data"
let lines = File.ReadAllLines(fileName)

let rowLength = Array.length lines
let colLength = lines[0].Length

let fold2D folder state arr2D =
    seq {
        for row in 0..(Array2D.length1 arr2D)-1 do
            for col in 0..(Array2D.length2 arr2D)-1 do
                yield row,col
    } |> Seq.fold folder state


let (mapData : char[,]), startPoint, endPoint =
    let map : char[,] = Array2D.zeroCreate rowLength colLength
    map
    |> fold2D (fun (m: char[,],s,e) (r,c) ->
        let value = lines[r][c]
        let height =
            if value = 'S' then
                'a'
            elif value = 'E' then
                'z'
            else
                value
        let start' = if value = 'S' then (r,c) else s
        let end' = if value = 'E' then (r,c) else e
        m[r,c] <- height
        (m,start',end')
        ) (map,(0,0),(0,0))

