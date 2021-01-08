module Sanchez.AOC.Solutions._2020.Day15

open System
open Sanchez.AOC.Core

let rec takeTurn maxTurn (spokenWords: Map<int, int>) i currentNumber =
    if i = maxTurn then
        currentNumber
    else
        let newNumber =
            spokenWords
            |> Map.tryFind currentNumber
            |> Option.map (fun x -> i - x)
            |> Option.defaultValue 0

        takeTurn maxTurn (Map.add currentNumber i spokenWords) (i + 1) newNumber

let [<Solution("2020", "15", "a")>] partA () =
    let startingMap =
        Map.empty
        |> Map.add 2 1
        |> Map.add 0 2
        |> Map.add 1 3
        |> Map.add 9 4
        |> Map.add 5 5
    let res = takeTurn 2020 startingMap 6 19

    res
    |> sprintf "%d"

let [<Solution("2020", "15", "b")>] partB () =
    let startingMap =
        Map.empty
        |> Map.add 2 1
        |> Map.add 0 2
        |> Map.add 1 3
        |> Map.add 9 4
        |> Map.add 5 5
    let res = takeTurn 30000000 startingMap 6 19

    res
    |> sprintf "%d"