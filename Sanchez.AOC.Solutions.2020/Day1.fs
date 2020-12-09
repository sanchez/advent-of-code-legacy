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

let [<Solution("2020", "1", "b")>] partB () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.choose (fun x ->
            match Int32.TryParse(x) with
            | true, i -> Some i
            | _ -> None)
        |> Seq.toArray


    input
    |> Seq.map (fun x ->
        input |> Seq.map (fun y ->
            input |> Seq.map (fun z -> (x, y, z))))
    |> Seq.collect id
    |> Seq.collect id
    |> Seq.filter (fun (x, y, z) -> (x + y + z) = 2020)
    |> Seq.map (fun (x, y, z) -> x * y * z)
    |> Seq.head
    |> string