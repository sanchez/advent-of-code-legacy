open System

open System.IO
open Sanchez.AOC.Core
open Sanchez.AOC.Solutions

let rec loadPart (a: SolutionPartMap) =
    printfn "Available Parts:"
    a |> Map.toSeq |> Seq.iter (fun (SolutionPart a, _) -> printfn " - %s" a)
    printf "Please select a part: "
    let part = Console.ReadLine() |> SolutionPart
    printfn ""
    
    a
    |> Map.tryFind part
    |> function
        | Some x ->
            (part, x)
        | None ->
            printfn "Invalid part entered: '%A'" part
            loadPart a
        

let rec loadDay (a: SolutionDayMap) =
    printfn "Available Days:"
    a |> Map.toSeq |> Seq.iter (fun (SolutionDay a, _) -> printfn " - %s" a)
    printf "Please select a day: "
    let day = Console.ReadLine() |> SolutionDay
    printfn ""
    
    a
    |> Map.tryFind day
    |> function
        | Some x ->
            let (part, cb) = loadPart x
            (day, part, cb)
        | None ->
            printfn "Invalid day entered: '%A'" day
            loadDay a

let rec loadYear (a: SolutionMap) =
    printfn "Available Years:"
    a |> Map.toSeq |> Seq.iter (fun (SolutionYear a, _) -> printfn " - %s" a)
    printf "Please select a year: "
    let year = Console.ReadLine() |> SolutionYear
    printfn ""
    
    a
    |> Map.tryFind year
    |> function
        | Some x ->
            let (day, part, cb) = loadDay x
            (year, day, part, cb)
        | None ->
            printfn "Invalid year entered: '%A'" year
            loadYear a

let fileLocation = "cache/lastRun.json"
type LastRunInfo = { Year: SolutionYear; Day: SolutionDay; Part: SolutionPart }
let loadFromDisk a =
    if File.Exists fileLocation then
        File.ReadAllText fileLocation
        |> Microsoft.FSharpLu.Json.Default.tryDeserialize<LastRunInfo>
        |> function
            | Choice1Of2 x ->
                a
                |> Map.tryFind x.Year
                |> Option.bind (Map.tryFind x.Day)
                |> Option.bind (Map.tryFind x.Part)
                |> Option.map (fun cb -> (x.Year, x.Day, x.Part, cb))
            | Choice2Of2 x -> None
    else None
    
    
let writeToDisk (year, day, part) =
    if fileLocation |> Path.GetDirectoryName |> Directory.Exists |> not then
        fileLocation |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
    
    { LastRunInfo.Year = year; Day = day; Part = part }
    |> Microsoft.FSharpLu.Json.Default.serialize
    |> (fun x -> File.WriteAllText(fileLocation, x))

let rec interactiveMode a =
    let (year, day, part, cb) = loadYear a
    writeToDisk (year, day, part)
    
    let answer = cb()
    printfn "Received answer for (%A, %A, %A) of %s" year day part answer

[<EntryPoint>]
let main argv =
    let solutions =
        [
            _2018.Solutions.load()
            _2019.Solutions.load()
        ]
        |> List.fold Loader.mergeSolution Map.empty
        
    match loadFromDisk solutions with
    | Some (year, day, part, cb) ->
        printfn "Found cached version, running..."
        cb()
        |> printfn "Got answer for (%A, %A, %A) of %s" year day part
    | None -> printfn "Found no cached version, entering interactive"
        
    while true do
        interactiveMode solutions
    
    printfn "Hello World from F#!"
    0 // return an integer exit code
