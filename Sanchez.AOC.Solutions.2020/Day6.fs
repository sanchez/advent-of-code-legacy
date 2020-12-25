module Sanchez.AOC.Solutions._2020.Day6

open System
open Sanchez.AOC.Core

let isTrue a b =
    match (a, b) with
    | (true, true) -> true
    | _ -> false

let inline loadData () =
    let (currentData, history) =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.fold (fun (currentData, history) x ->
            if x.Length = 0 then
                ([], currentData::history)
            else (x::currentData, history)) ([], [])

    currentData::history
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.map (fun x -> String.Join("", x))
    |> Seq.toArray

let inline loadIndividualData () =
    let (currentData, history) =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.fold (fun (currentData, history) x ->
            if x.Length = 0 then
                ([], currentData::history)
            else (x::currentData, history)) ([], [])
    
    currentData::history
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.toArray

let [<Solution("2020", "6", "a")>] partA () =
    let input =
        loadData()
        |> Seq.map Seq.distinct
        |> Seq.collect id

    input
    |> Seq.length
    |> sprintf "%d"

let calculateEveryoneAnswered (answers: char seq seq) =
    if Seq.length answers < 2 then
        answers |> Seq.head
    else
        let others =
            answers
            |> Seq.tail
            |> Seq.map Seq.toArray
            |> Seq.toArray
        let scanAnswers c =
            others
            |> Seq.map (Seq.contains c)
            |> Seq.fold isTrue true

        answers
        |> Seq.head
        |> Seq.filter scanAnswers

let [<Solution("2020", "6", "b")>] partB () =
    let input =
        loadIndividualData()
        |> Seq.map (
            Seq.cast
            >> Seq.map Seq.cast
            >> calculateEveryoneAnswered
            >> Seq.toArray)
        |> Seq.toArray

    input
    |> Seq.map (Seq.length)
    |> Seq.reduce (+)
    |> sprintf "%d"