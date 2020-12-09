module Sanchez.AOC.Solutions._2020.Day2

open System
open Sanchez.AOC.Core

let [<Solution("2020", "2", "a")>] partA () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> x.Length > 0)
        |> Seq.map (sscanf "%d-%d %c: %s")
        |> Seq.toArray

    let isValid =
        input
        |> Seq.filter (fun (min, max, c, password) ->
            password
            |> Seq.filter ((=) c)
            |> Seq.length
            |> (fun x -> (x >= min) && (x <= max)))

    isValid |> Seq.length |> string

let [<Solution("2020", "2", "b")>] partB () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.filter (fun x -> x.Length > 0)
        |> Seq.map (sscanf "%d-%d %c: %s")
        |> Seq.toArray

    let isValid =
        input
        |> Seq.filter (fun (first, second, c, password) ->
            let f = password.[first - 1]
            let s = password.[second - 1]
            if (f = c) && (s = c) then false
            elif f = c then true
            elif s = c then true
            else false)

    isValid |> Seq.length |> string