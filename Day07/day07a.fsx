open System.IO

let fileName = "Day07/day07.test.data"
let lines = File.ReadAllLines(fileName)

type DirectoryEntry =
        | DirectoryNode of string * DirectoryEntry option * DirectoryEntry list
        | FileNode of string * DirectoryEntry option * int

let fileSystem = DirectoryNode ("/", None, [])
let rootDir = fileSystem

let addFile (fileName: string) (size: int) (dir: DirectoryEntry) : DirectoryEntry =
    let currName, currParent, currEntries = 
        match dir with
        | DirectoryNode (n,p,e) -> n,p,e
        | _ -> failwith "Invalid currentDirectory - not a directory"

    if currEntries |> List.exists (fun de ->
                                    match de with
                                    | DirectoryNode (name,_,_) -> name = fileName
                                    | FileNode (name,_,_) -> name = fileName
                                  ) then
        failwithf "File already exists: %s" fileName
    
    let rec x = DirectoryNode (currName, currParent, (FileNode(fileName,Some x,size))::currEntries)
    x


let cd (target: string) (currentDirectory: DirectoryEntry) =
    match target with
    | "/" ->
        let rec loop curr =
            let _, parent, _ =
                match curr with
                | DirectoryNode (n,p,e) -> n,p,e
                | _ -> failwith "Invalid currentDirectory - not a directory"
            match parent with
            | None -> curr // No parent so we must be at /
            | Some p -> loop p
        loop currentDirectory
    | _ ->
        let _, _, currEntries =
            match currentDirectory with
            | DirectoryNode (n,p,e) -> n,p,e
            | _ -> failwith "Invalid currentDirectory - not a directory"
        currEntries
        |> List.find (fun entry ->
                        match entry with
                        | DirectoryNode(name,_,_) -> name = target
                        | _ -> false
                        )

