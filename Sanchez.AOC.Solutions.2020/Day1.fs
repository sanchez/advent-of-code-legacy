module Sanchez.AOC.Solutions._2020.Day1

open System
open Sanchez.AOC.Core

let [<Solution("2020", "1", "a")>] partA () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.choose (fun x ->
            match Int32.TryParse(x) with
            | true, i -> Some i
            | _ -> None)
        |> Seq.toArray

    let (left, right) =
        input
        |> Seq.choose (fun x ->
            input
            |> Seq.tryFind ((+) x >> ((=) 2020))
            |> Option.map (fun y -> (x, y)))
        |> Seq.head

    left * right
    |> string