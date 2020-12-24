module Sanchez.AOC.Solutions._2020.Day3

open Sanchez.AOC.Core

type PossibleItem =
    | Tree
    | Spacing

let inline loadInput () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.map (
            Seq.choose (
                function
                | '.' -> Some Spacing
                | '#' -> Some Tree
                | _ -> None)
            >> Seq.toArray)
        |> Seq.filter (fun x -> x.Length <> 0)
        |> Seq.toArray

    let getItem (x, y) =
        if y >= input.Length then None
        else
            let row = input.[y]
            Some row.[x % row.Length]

    getItem

let [<Solution("2020", "3", "a")>] partA () =
    let getItem = loadInput()

    let movePos (x, y) = (x + 3, y + 1)

    let rec getItems pos =
        match getItem pos with
        | Some x -> x::(pos |> movePos |> getItems)
        | None -> []

    let items = getItems (0, 0)

    let trees =
        items
        |> Seq.filter (function
            | Tree -> true
            | Spacing -> false)
        |> Seq.length

    sprintf "%d" trees

let [<Solution("2020", "3", "b")>] partB () =
    let getItem = loadInput()

    let movePos (mX, mY) (x, y) = (x + mX, y + mY)

    let rec getItems incre pos =
        match getItem pos with
        | Some x -> x::(pos |> movePos incre |> getItems incre)
        | None -> []

    let movements =
        [
            (1, 1)
            (3, 1)
            (5, 1)
            (7, 1)
            (1, 2)
        ]

    let trees =
        movements
        |> Seq.map getItems
        |> Seq.map ((|>) (0, 0))
        |> Seq.map (
            Seq.filter (
                function
                | Tree -> true
                | Spacing -> false)
            >> Seq.length)
        |> Seq.reduce (*)

    sprintf "%d" trees