open System.IO
open System.Linq

let fileName = "Day07/day07.data"
let lines = File.ReadAllLines(fileName)

type DirectoryEntry =
        | DirectoryNode of string * DirectoryEntry option * ResizeArray<DirectoryEntry>
        | FileNode of string * DirectoryEntry option * int

let addFile (fileName: string) (size: int) (dir: DirectoryEntry) : unit =
    let currEntries = 
        match dir with
        | DirectoryNode (_,_,e) -> e
        | _ -> failwith "Invalid currentDirectory - not a directory"

    if currEntries.Any (fun de -> match de with
                                  | DirectoryNode (name,_,_) -> name = fileName
                                  | FileNode (name,_,_) -> name = fileName
                                  ) then
        failwithf "File or directory already exists with the name %s" fileName
    
    currEntries.Add(FileNode(fileName,Some dir,size))
    ()

let addDirectory(fileName: string) (dir: DirectoryEntry) : unit =
    let currEntries = 
        match dir with
        | DirectoryNode (_,_,e) -> e
        | _ -> failwith "Invalid currentDirectory - not a directory"

    if currEntries.Any (fun de -> match de with
                                  | DirectoryNode (name,_,_) -> name = fileName
                                  | FileNode (name,_,_) -> name = fileName
                                  ) then
        failwithf "File or directory already exists with the name %s" fileName
    
    currEntries.Add(DirectoryNode(fileName,Some dir,ResizeArray()))
    ()

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

    | ".." ->
        let _, parent, _ =
            match currentDirectory with
            | DirectoryNode (n,p,e) -> n,p,e
            | _ -> failwith "Invalid currentDirectory - not a directory"
        match parent with
        | Some p -> p
        | _ -> failwith "Already at root directory"

    | _ ->
        let _, _, currEntries =
            match currentDirectory with
            | DirectoryNode (n,p,e) -> n,p,e
            | _ -> failwith "Invalid currentDirectory - not a directory"
        currEntries.Find (fun entry ->
                            match entry with
                            | DirectoryNode(name,_,_) -> name = target
                            | _ -> false
                            )

// Initialise filesystem
let fileSystem = DirectoryNode ("/", None, ResizeArray<DirectoryEntry>())
let rootDir = fileSystem

let loadedFileSystem, _ =
    lines
    |> Array.fold (fun (fs,currDir) line ->
        if line.StartsWith("$ cd ") then
            // Make directory in current directory if it doesn't already exist then set current to new directory (unless it's / then just go to it)
            let dirName = line[5..]
            if dirName <> "/" && dirName <> ".." then
                let names =
                    match currDir with
                    | DirectoryNode (_,_,es) ->
                        es.Select(fun entry -> match entry with
                                               | DirectoryNode (name,_,_) -> name
                                               | FileNode (name,_,_) -> name)
                    | _ -> failwith "Current directory is not a DirectoryNode"
                if names.All(fun n -> n <> dirName) then
                    addDirectory dirName currDir
            let newDir = cd dirName currDir
            (fs,newDir)
        elif line = "$ ls" then
            (fs,currDir)
        elif line.StartsWith("$") then
            failwithf "Unknown command: %s" line
        elif line.StartsWith("dir ") then
            let newDirName = line[4..]
            addDirectory newDirName currDir
            (fs,currDir)
        else
            let size,name =
                match line.Split(' ') with
                | [| s ; n |] -> (int s),n
                | _ -> failwithf "Invalid line: %s" line
            addFile name size currDir
            (fs,currDir)
        ) (fileSystem, rootDir)

let getDirectorySizes (directory: DirectoryEntry) : (string*int) list =
    let rec loop (dir:DirectoryEntry) : (string*int) list=
        // Total of all files + total of subdirectories
        // return my total + total for each subdir
        let name, entries =
            match dir with
            | DirectoryNode (n, _, es) -> n,es
            | _ -> failwith "Not a DirectoryNode"
        let filesTotal =
            entries
            |> Seq.sumBy (function
                          | FileNode (_,_,size) -> size
                          | _ -> 0)
        let subDirs =
            entries
            |> Seq.choose (fun entry ->
                            match entry with
                            | DirectoryNode (_,_,_) -> Some entry
                            | _ -> None)
            |> Seq.fold (fun state entry -> loop entry @ state) []

        let subDirsTotal =
            subDirs
            |> Seq.sumBy(snd)
        
        (name,(filesTotal+subDirsTotal))::subDirs
    loop directory

let sizes = getDirectorySizes rootDir

let result =
    sizes
    |> Seq.filter (fun (_,s) -> s <= 100000)
    |> Seq.sumBy (snd)

printfn "Result: %i" result
