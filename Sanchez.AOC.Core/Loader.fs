namespace Sanchez.AOC.Core

open System
open System.Reflection

module Loader =
    let load() =
        let test =
            AppDomain.CurrentDomain.GetAssemblies()
            |> Seq.map (fun x -> x.GetTypes())
            |> Seq.collect id
            |> Seq.map (fun t ->
                t.GetMethods(BindingFlags.Static)
                |> Seq.map (fun x -> x.GetCustomAttributes(typeof<SolutionAttribute>, true)))
            |> Seq.collect id
            |> Seq.toList
        
        printfn "Hello World"
        ()