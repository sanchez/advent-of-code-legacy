module Sanchez.AOC.Solutions._2018.Day1

open System
open Sanchez.AOC.Core

let [<Solution("2018", "1", "a")>] partA () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.choose (fun x ->
            match Int32.TryParse(x) with
            | true, i -> Some i
            | _ -> None)
        |> Seq.fold (+) 0
    
    input |> string
    
let [<Solution("2018", "1", "b")>] partB() =
    let mutable set = Set.empty
    
    let test =
        [ -6; 3; 8; 5; -6 ]
        |> Seq.scan (+) 0
        |> Seq.toArray
    
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.choose (fun x ->
            match Int32.TryParse(x) with
            | true, i -> Some i
            | _ -> None)
        |> Seq.toArray
        
    let inifiniteInput =
        Seq.initInfinite (fun i -> input.[i % input.Length])
        |> Seq.scan (+) 0
        
    let repeatedValue =
        inifiniteInput
        |> Seq.find (fun x ->
            if Set.contains x set then
                true
            else
                set <- Set.add x set
                false)
        
    repeatedValue |> string