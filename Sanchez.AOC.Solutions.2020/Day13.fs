module Sanchez.AOC.Solutinos._2020.Day13

open System
open Sanchez.AOC.Core

let bothSuccess a b =
    match (a, b) with
    | (true, true) -> true
    | _ -> false

let createBusSchedule maxValue busId =
    Seq.init (maxValue / busId) (fun i -> (busId, busId * i))

let inline loadData () =
    let lines = InputLoader.load().Split("\n")
    let buses = 
        lines.[1].Split(",")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> x <> "x")
        |> Seq.map int
        |> Seq.toArray
    (int lines.[0], buses)

let [<Solution("2020", "13", "a")>] partA () =
    let (estimatedTime, buses) =
        loadData()

    let (bus, depart) =
        buses
        |> Seq.map (createBusSchedule (estimatedTime * 2))
        |> Seq.collect id
        |> Seq.filter (fun (_, x) -> x >= estimatedTime)
        |> Seq.sortBy snd
        |> Seq.head

    let waiting = depart - estimatedTime


    bus * waiting
    |> sprintf "%d"

type ConstraintType =
    | Bus of int
    | UnContrainted
module ConstraintType =
    let busApply f =
        function
        | Bus x -> f x
        | UnContrainted -> failwithf "not a bus"
    let busValue = busApply id

let isConstraintSuccess timestamp =
    function
    | Bus x -> (timestamp % x) = 0
    | UnContrainted -> true

let inline loadPartBData () =
    InputLoader.load().Split("\n").[1].Split(",")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.map (function
        | "x" -> UnContrainted
        | x -> x |> int |> Bus)
    |> Seq.toArray

let [<Solution("2020", "13", "b")>] partB () =
    let data =
        loadPartBData()

    printfn "Loaded: %d" data.Length

    let leader = ConstraintType.busValue data.[0]
    let leaderSchedule = Seq.initInfinite (fun i -> i * leader)

    let testing =
        data
        |> Seq.mapi (fun i x -> (i, x))
        |> Seq.choose (fun (i, x) ->
            match x with
            | Bus b -> b + i |> Some
            | UnContrainted -> None)
        |> Seq.toArray
    let totalTesting =
        testing
        |> Seq.reduce (*)

    let firstTimestamp =
        leaderSchedule
        |> Seq.filter (fun x ->
            let closeness =
                data
                |> Seq.mapi (fun i x -> (i, x))
                |> Seq.filter (fun (i, constr) -> isConstraintSuccess (x + i) constr)
                |> Seq.toArray

            printfn "Trying %d resulting: %d" x closeness.Length

            closeness.Length = data.Length)
        |> Seq.head

    firstTimestamp
    |> sprintf "%d"