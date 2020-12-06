namespace Sanchez.AOC.Core

open System
open System.Reflection

type SolutionCallback = unit -> string
type SolutionPartMap = Map<SolutionPart, SolutionCallback>
type SolutionDayMap = Map<SolutionDay, SolutionPartMap>
type SolutionMap = Map<SolutionYear, SolutionDayMap>

module Loader =
    let loadMethodCallback (m: MethodInfo) () =
        m.Invoke(null, BindingFlags.DoNotWrapExceptions, null, null, null)
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
        
    let private mergeSinglePart (a: SolutionPartMap) (k, v) = Map.add k v a
        
    let private mergePart (a: SolutionPartMap) (b: SolutionPartMap) =
        Map.fold (fun acc part cb -> mergeSinglePart acc (part, cb)) b a
        
    let private mergeSingleDay (a: SolutionDayMap) (k, v) =
        a
        |> Map.tryFind k
        |> function
            | Some x -> mergePart x v
            | None -> v
        |> (fun x -> Map.add k x a)
        
    let private mergeDay (a: SolutionDayMap) (b: SolutionDayMap) =
        Map.fold (fun acc day x -> mergeSingleDay acc (day, x)) b a
        
    let private mergeSingleYear (a: SolutionMap) (k, v) =
        a
        |> Map.tryFind k
        |> function
            | Some x -> mergeDay x v
            | None -> v
        |> fun x -> Map.add k x a
        
    let mergeSolution (a: SolutionMap) (b: SolutionMap) =
        Map.fold (fun acc year x -> mergeSingleYear acc (year, x)) b a