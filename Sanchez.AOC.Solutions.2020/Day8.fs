module Sanchez.AOC.Solutions._2020.Day8

open System
open Sanchez.AOC.Core

type Instruction =
    | Accumulator of int
    | Jump of int
    | NoOp of int

let inline loadData () =
    InputLoader.load().Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.choose (
        function
        | Regex "^acc ([-+]\d+)$" [num] -> num |> int |> Accumulator |> Some
        | Regex "^jmp ([-+]\d+)$" [num] -> num |> int |> Jump |> Some
        | Regex "^nop ([-+]\d+)$" [num] -> num |> int |> NoOp |> Some
        | x ->
            failwithf "Failed to match for: %s" x
            None)
    |> Seq.toArray

let [<Solution("2020", "8", "a")>] partA () =
    let instr =
        loadData()

    let executeInstruction (acc, pos) =
        function
        | Accumulator a -> (acc + a, pos + 1)
        | Jump a -> (acc, pos + a)
        | NoOp _ -> (acc, pos + 1)
    
    let rec step visited (acc, pos) =
        let (newAcc, newPos) = instr.[pos] |> executeInstruction (acc, pos)

        if Set.contains newPos visited then
            newAcc
        else
            let newVisited = Set.add newPos visited
            step newVisited (newAcc, newPos)

    let visited = Set.empty |> Set.add 0
    let loopedAcc = step visited (0, 0)

    loopedAcc
    |> sprintf "%d"

let [<Solution("2020", "8", "b")>] partB () =
    let instr = loadData()

    let rec step hasChanged visited (acc, pos): int option =
        if pos >= instr.Length then Some acc
        elif visited |> Set.contains pos then None
        else
            let ins = instr.[pos]

            let newVisited = visited |> Set.add pos

            match ins with
            | Accumulator a -> step hasChanged newVisited (acc + a, pos + 1)
            | Jump a ->
                step hasChanged newVisited (acc, pos + a) // jump
                |> function
                    | Some x -> Some x
                    | None ->
                        if hasChanged then None
                        else step true newVisited (acc, pos + 1) // noop
            | NoOp a ->
                step hasChanged newVisited (acc, pos + 1) // noop
                |> function
                    | Some x -> Some x
                    | None ->
                        if hasChanged then None
                        else step true newVisited (acc, pos + a)

    let finalAcc = step false Set.empty (0, 0)

    sprintf "%A" finalAcc