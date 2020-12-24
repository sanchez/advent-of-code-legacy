module Sanchez.AOC.Solutions._2020.Day5

open System
open Sanchez.AOC.Core

let midPoint a b =
    (a + b) / 2

let rec getRowPos a b poses =
    let mid = midPoint a b
    match poses with
    | 'F'::rem -> getRowPos a mid rem
    | 'B'::rem -> getRowPos (mid + 1) b rem
    | _ ->
        if a = b then a
        else failwithf "Failed to find a position"

let rec getColumnPos a b poses =
    let mid = midPoint a b
    match poses with
    | 'L'::rem -> getColumnPos a mid rem
    | 'R'::rem -> getColumnPos (mid + 1) b rem
    | _ ->
        if a = b then a
        else failwithf "Failed to find a position"

let inline loadData () =
    InputLoader.load().Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.Length > 0)
    |> Seq.map (fun x -> (x.[..6] |> Seq.toList, x.[7..] |> Seq.toList))
    |> Seq.toArray

let tryFind key m =
    Map.tryFind key m
    |> Option.defaultValue false

let [<Solution("2020", "5", "a")>] partA () =
    let input =
        loadData()
        |> Seq.map (fun (a, b) ->
            let row = getRowPos 0 127 a
            let column = getColumnPos 0 7 b
            
            (row, column))
        |> Seq.map (fun (a, b) -> a * 8 + b)
        |> Seq.max

    sprintf "%d" input

let [<Solution("2020", "5", "b")>] partB () =
    let input =
        loadData()
        |> Seq.map (fun (a, b) ->
            let row = getRowPos 0 127 a
            let column = getColumnPos 0 7 b
            
            (row, column))
        |> Seq.map (fun (a, b) -> a * 8 + b)
        |> Seq.map (fun x -> (x, true))
        |> Map.ofSeq

    let seats = 
        Seq.init 1023 id
        |> Seq.filter (fun x -> Map.containsKey x input |> not)
        |> Seq.filter (fun x ->
            let left = tryFind (x - 1) input
            let right = tryFind (x + 1) input
            let res = left && right
            left && right)
        |> Seq.toArray
        |> Seq.head

    sprintf "%d" seats