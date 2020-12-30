module Sanchez.AOC.Solutions._2020.Day10

open System
open Sanchez.AOC.Core

type ExploredPath = ExploredPath of int list
module ExploredPath =
    let apply f (ExploredPath a) = f a
    let value = apply id
    let add a (ExploredPath p) = a::p |> ExploredPath

let inline loadData () =
    InputLoader.load().Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.map int
    |> Seq.append [0]
    |> Seq.sort
    |> Seq.toArray

let [<Solution("2020", "10", "a")>] partA () =
    let counts =
        loadData()
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> b - a)
        |> Seq.countBy id
        |> Seq.toArray

    let ones = counts |> Seq.find (fst >> ((=) 1)) |> snd
    let threes = counts |> Seq.find (fst >> ((=) 3)) |> snd

    ones * (threes + 1)
    |> sprintf "%d"

let [<Solution("2020", "10", "b")>] partB () =
    let rawData = loadData()
    let computer = rawData |> Seq.max
    let data =
        Set.ofArray rawData
        |> Set.add computer
    let outlet = 0

    let getPossibleOutlets visits input =
        [input .. (input + 2)]
        |> Seq.map ((+) 1)
        |> Seq.filter (fun x -> Set.contains x visits |> not)
        |> Seq.filter (fun x -> Set.contains x data)
        |> Seq.toList

    let rec getPath visits input =
        let newVisits = Set.add input visits
        if input = computer then
            [newVisits]
        else
            let possible = getPossibleOutlets newVisits input

            let paths =
                possible
                |> Seq.map (getPath newVisits)
                |> Seq.collect id
                |> Seq.toList

            paths

    let test = getPossibleOutlets Set.empty 0

    let finalPaths = 
        getPath Set.empty 0

    finalPaths
    |> List.length
    |> sprintf "%d"