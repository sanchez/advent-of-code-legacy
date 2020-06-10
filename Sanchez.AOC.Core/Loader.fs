namespace Sanchez.AOC.Core

open System
open System.Reflection

module Loader =
    let loadMethodCallback (m: MethodInfo) () =
        m.Invoke(null, null)
        :?> string
    
    let loadTypeCallers (t: Type) =
        t.GetMethods()
        |> Seq.filter (fun x -> x.IsStatic)
        |> Seq.map (fun x ->
            x.GetCustomAttributes(typeof<SolutionAttribute>, false)
            |> Array.map (fun a -> (x |> loadMethodCallback, a :?> SolutionAttribute)))
        |> Seq.collect id
        |> Seq.toArray
    
    let load() =
        AppDomain.CurrentDomain.GetAssemblies()
        |> Seq.filter (fun x -> x.GetName().Name.StartsWith("System") |> not)
        |> Seq.map (fun x ->
            x.GetTypes()
            |> Seq.map (loadTypeCallers)
            |> Seq.collect id)
        |> Seq.collect id
        |> Seq.groupBy (fun (fn, s) -> s.Year |> SolutionYear)
        |> Map.ofSeq
        |> Map.map (fun _ -> Seq.groupBy (fun (fn, s) -> s.Day |> SolutionDay) >> Map.ofSeq)
        |> Map.map (fun _ -> Map.map (fun _ -> Seq.map (fun (fn, x) -> (x.Part |> SolutionPart, fn)) >> Map.ofSeq))