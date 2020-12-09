module Sanchez.AOC.Core.InputLoader

open System.Diagnostics
open System.IO

let private inputFolderLocation = "inputs/"

let load () =
    let stackFrame = new StackFrame(1)
    let method = stackFrame.GetMethod()
    let solutionInfo =
        method.CustomAttributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<SolutionAttribute>)
        |> Option.defaultWith (fun () -> failwith "Failed to find solution information")
        
    let loadedInfo =
        solutionInfo.Constructor.Invoke(solutionInfo.ConstructorArguments |> Seq.map (fun x -> x.Value) |> Seq.toArray)
        :?> SolutionAttribute
    
    let finalPath = sprintf "inputs/%s/%s-input.txt" loadedInfo.Year loadedInfo.Day
    let dir = finalPath |> Path.GetDirectoryName
    if dir |> Directory.Exists |> not then
        dir |> Directory.CreateDirectory |> ignore

    let test = Path.GetFullPath(finalPath)
        
    if finalPath |> File.Exists |> not then
        failwith "Input file does not exist"
        
    finalPath
    |> File.ReadAllText