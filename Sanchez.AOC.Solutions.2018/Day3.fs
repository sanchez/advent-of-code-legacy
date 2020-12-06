module Sanchez.AOC.Solutions._2018.Day3

open Sanchez.AOC.Core

type Point =
    {
        X: int
        Y: int
    }

let [<Solution("2018", "3", "a")>] partA () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.choose (trySscanf "#%d @ %d,%d: %dx%d")
        |> Seq.map (fun (id, left, right, width, height) ->
            seq {
                for x in left .. (left + width - 1) do
                    for y in right .. (right + height - 1) do
                        yield { X = x; Y = y }
            })
        |> Seq.collect id
        |> Seq.groupBy id
        |> Seq.map (fun (pt, items) -> (pt, items |> Seq.length))
        |> Seq.filter (fun (_, items) -> items > 1)
        |> Seq.toArray
        
    input
    |> Array.length
    |> sprintf "%d"
    
let [<Solution("2018", "3", "b")>] partB () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.choose (trySscanf "#%d @ %d,%d: %dx%d")
        |> Seq.map (fun (id, left, right, width, height) ->
            (id,
             seq {
                for x in left .. (left + width - 1) do
                    for y in right .. (right + height - 1) do
                        yield (id, { X = x; Y = y })
            }))
        |> Seq.cache
        
    let points =
        input
        |> Seq.map snd
        |> Seq.collect id
        |> Seq.groupBy snd
        |> Seq.filter (fun (_, items) -> (items |> Seq.length) > 1)
        |> Seq.map (snd >> Seq.map fst)
        |> Seq.collect id
        |> Seq.distinct
        |> Seq.toArray
        
    let noOverlappingIds =
        input
        |> Seq.map fst
        |> Seq.filter (fun x -> Array.tryFind ((=) x) points |> Option.isNone)
        
    noOverlappingIds
    |> Seq.exactlyOne
    |> sprintf "%d"