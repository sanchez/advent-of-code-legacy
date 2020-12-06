module Sanchez.AOC.Solutions._2018.Day5

open Sanchez.AOC.Core

let areBasicPolarity (a: char) (b: char) = System.Char.ToUpper(a) = b || System.Char.ToUpper(b) = a

let rec removePolarity arePolarity (chain: char list) =
    match chain with
    | a::b::rem when arePolarity a b -> rem |> removePolarity arePolarity
    | a::rem -> a::(removePolarity arePolarity rem)
    | [] -> []
    
let rec recursivelyRemove arePolarity (chain: char list) =
    let newChain = removePolarity arePolarity chain
    if (newChain |> List.length) <> (chain |> List.length) then
        recursivelyRemove arePolarity newChain
    else newChain

let [<Solution("2018", "5", "a")>] partA() =
    let example =
        "dabAcCaCBAcCcaDA"
        |> Seq.toList
        |> recursivelyRemove areBasicPolarity
    
    let input =
        InputLoader.load().Trim()
        |> Seq.toList
        |> recursivelyRemove areBasicPolarity
        |> List.toArray
    
    input
    |> Array.length
    |> string