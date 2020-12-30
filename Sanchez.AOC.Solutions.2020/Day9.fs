module Sanchez.AOC.Solutions._2020.Day9

open System
open Sanchez.AOC.Core

let inline loadData () =
    InputLoader.load().Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.map int64
    |> Seq.toArray

let allCombinations a =
    a
    |> Seq.map (fun left ->
        a
        |> Seq.map (fun right -> (left, right)))
    |> Seq.collect id

let findPairing items targetNum =
    items
    |> allCombinations
    |> Seq.tryFind (fun (a, b) -> (a + b) = targetNum)

let inline invalidNumber () =
    let input = loadData()

    let isValid pos =
        if pos < 25 then true
        else
            let start = pos - 25
            let validInput = input.[start..(pos - 1)]
            let currentVal = input.[pos]

            findPairing validInput currentVal
            |> Option.isSome
    
    let valid =
        Seq.init input.Length id
        |> Seq.filter (isValid >> not)
        |> Seq.toArray

    valid
    |> Seq.head
    |> (fun x -> (input, input.[x]))

let [<Solution("2020", "9", "a")>] partA () =
    invalidNumber()
    |> snd
    |> sprintf "%d"

let [<Solution("2020", "9", "b")>] partB () =
    let (input, invalidNumber) = invalidNumber()

    let resultingSet =
        seq {
            for i in 0 .. (input.Length - 1) do
                for j in i .. (input.Length - 1) do
                    yield input.[i..j]
        }
        |> Seq.map (fun x -> (Seq.reduce (+) x, x))
        |> Seq.filter (fst >> ((=) invalidNumber))
        |> Seq.filter (snd >> Seq.contains invalidNumber >> not)
        |> Seq.head
        |> snd

    let min = Seq.min resultingSet
    let max = Seq.max resultingSet

    min + max
    |> sprintf "%d"