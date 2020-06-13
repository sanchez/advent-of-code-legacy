module Sanchez.AOC.Solutions._2018.Day2

open System
open Sanchez.AOC.Core

let getCharacterCounts (a: string) =
    a
    |> Seq.groupBy (id)
    |> Seq.map (fun (c, s) -> (c, s |> Seq.length))
    |> Seq.toArray
    
let hasCharactCount count (a: (char * int) []) =
    a
    |> Seq.filter (snd >> ((=) count))
    |> Seq.tryHead
    |> function
        | Some _ -> true
        | None -> false

let [<Solution("2018", "2", "a")>] partA () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.cache
        
    let twoLetters =
        input
        |> Seq.filter (getCharacterCounts >> hasCharactCount 2)
        |> Seq.length
        
    let threeLetters =
        input
        |> Seq.filter (getCharacterCounts >> hasCharactCount 3)
        |> Seq.length
        
    let checksum = twoLetters * threeLetters
    
    checksum |> string
    
let similarity (a: string) (b: string) =
    Seq.zip a b
    |> Seq.filter (fun (x, y) -> x <> y)
    |> Seq.length
    
let commonLetters (a: string) (b: string) =
    Seq.zip a b
    |> Seq.filter (fun (x, y) -> x = y)
    |> Seq.map fst
    |> Seq.toArray
    |> String
    
let [<Solution("2018", "2", "b")>] partB () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.toArray
        
    let similar =
        input
        |> Seq.choose (fun x ->
            input
            |> Seq.tryFind (similarity x >> (=) 1)
            |> Option.map (fun a -> (x, a)))
        |> Seq.head
        |> (fun (a, b) -> commonLetters a b)
    
    similar