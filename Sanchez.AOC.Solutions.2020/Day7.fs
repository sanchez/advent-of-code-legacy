module Sanchez.AOC.Solutions._2020.Day7

open System
open Sanchez.AOC.Core

type BagColor = BagColor of string
module BagColor =
    let apply f (BagColor a) = f a
    let value = apply id

type BagRule =
    {
        Coloring: BagColor
        Contains: (int * BagColor) list
    }

let convertLine s =
    let res = tryRegex "^([\w\s]+) bags contain (.*)\.$" s |> Option.get
    let color = res.[0] |> BagColor

    let contains =
        if res.[1] = "no other bags" then []
        else
            res.[1].Split(",")
            |> Seq.map (fun x -> x.Trim())
            |> Seq.map (fun x ->
                tryRegex "^(\d+) (.*) bags?$" x
                |> Option.get)
            |> Seq.map (fun x -> (int x.[0], BagColor x.[1]))
            |> Seq.toList

    {
        BagRule.Coloring = color
        Contains = contains
    }

let inline loadData () =
    InputLoader.load().Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.map convertLine
    |> Seq.toArray

let [<Solution("2020", "7", "a")>] partA () =
    let input =
        loadData()
        |> Seq.map (fun x -> (x.Coloring, x.Contains))
        |> Map.ofSeq

    let findColor x = Map.find x input

    let rec loadParents (BagColor x): BagColor seq =
        let parents =
            input
            |> Map.toSeq
            |> Seq.choose (fun (p, c) ->
                c
                |> Seq.filter (snd >> BagColor.value >> ((=) x))
                |> Seq.tryHead
                |> Option.map (fun _ -> p))

        let higherParents =
            parents
            |> Seq.map loadParents
            |> Seq.collect id

        Seq.append parents higherParents

    let test =
        loadParents (BagColor "shiny gold")
        |> Seq.distinct
        |> Seq.toArray

    test
    |> Array.length
    |> sprintf "%d"

let [<Solution("2020", "7", "b")>] partB () =
    let input =
        loadData()
        |> Seq.map (fun x -> (x.Coloring, x.Contains))
        |> Map.ofSeq

    let rec loadInnerBags x:int =
        let items =
            input
            |> Map.find x

        if items.Length > 0 then
            items
            |> Seq.map (fun (a, b) -> a * (loadInnerBags b))
            |> Seq.reduce (+)
            |> ((+) 1)
        else 1

    loadInnerBags (BagColor "shiny gold")
    |> (fun x -> x - 1)
    |> sprintf "%d"