module Sanchez.AOC.Solutions._2020.Day11

open System
open Sanchez.AOC.Core

type SeatState =
    | Occupied
    | Empty
    | Floor
type SeatMap = Map<(int * int), SeatState>

let getPrint (state: Map<int * int, SeatState>) =
    let rowMax = state |> Map.toSeq |> Seq.map (fst >> fst) |> Seq.max
    let columnMax = state |> Map.toSeq |> Seq.map (fst >> snd) |> Seq.max

    let stateChar =
        function
        | Occupied -> '#'
        | Empty -> 'L'
        | Floor -> '.'

    seq {
        for x in [0 .. rowMax] do
            for y in [0 .. columnMax] do
                yield
                    state
                    |> Map.tryFind (x, y)
                    |> Option.map stateChar
                    |> Option.defaultValue '-'
            yield '\n'
    }
    |> String.Concat

let getSurrounding (seatState: SeatMap) (x, y) search =
    let itemCounts =
        seq {
            for i in [(x - 1) .. (x + 1)] do
                for j in [(y - 1) .. (y + 1)] do
                    if i <> x && j <> y then
                        yield (i, j)
        }
        |> Seq.choose (fun x -> Map.tryFind x seatState)
        |> Seq.countBy id
        |> Map.ofSeq
    itemCounts
    |> Map.tryFind search
    |> Option.defaultValue 0

let updateSeat seatState (pos, currentState) =
    let surrounding = getSurrounding seatState pos Occupied
    let newState =
        match currentState with
        | Floor -> Floor
        | Empty ->
            if surrounding = 0 then Occupied
            else Empty
        | Occupied ->
            if surrounding >= 4 then Empty
            else Occupied
    (pos, newState)

let updateSeating (seatState: SeatMap) =
    seatState
    |> Map.toSeq
    |> Seq.map (updateSeat seatState)
    |> Map.ofSeq

let compareStates (previous: Map<int * int, SeatState>) (next: Map<int * int, SeatState>) =
    let res =
        next
        |> Map.toSeq
        |> Seq.filter (fun (pos, newState) ->
            let oldState = Map.find pos previous
            newState <> oldState)
        |> Seq.toArray
    printfn "New Resolve: %d" res.Length
    res.Length = 0

let processData (s: string) =
    s.Split("\n")
    |> Seq.mapi (fun i ->
        Seq.choose (
            function
            | 'L' -> Some Empty
            | '#' -> Some Occupied
            | '.' -> Some Floor
            | c ->
                failwithf "Yeah nah yeah gg: %c" c
                None
        )
        >> Seq.mapi (fun j x -> ((j, i), x))
    )
    |> Seq.collect id
    |> Map.ofSeq

let inline loadData () =
    InputLoader.load()
    |> processData

let testData () =
    """
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"""
    |> processData

let testing () =
    let seating = testData()

    let getSurrounding (seatState: Map<(int * int), SeatState>) (x, y) =
        seq {
            for i in [(x - 1) .. (x + 1)] do
                for j in [(y - 1) .. (y + 1)] do
                    if i <> 0 && j <> 0 then
                        yield (i, j)
        }
        |> Seq.choose (fun x -> Map.tryFind x seatState)
        |> Seq.countBy id
        |> Map.ofSeq

    let getSurroundFunc seatState pos =
        let surround = getSurrounding seatState pos
        (fun x ->
            surround
            |> Map.tryFind x
            |> Option.defaultValue 0)

    let updateSeating (seatState: Map<(int * int), SeatState>) =
        seatState
        |> Map.toSeq
        |> Seq.map (fun (pos, state) ->
            let newState =
                match state with
                | Floor -> Floor
                | Empty ->
                    let surrounding = getSurroundFunc seatState pos Occupied
                    if surrounding = 0 then Occupied
                    else Empty
                | Occupied ->
                    let surrounding = getSurroundFunc seatState pos Occupied
                    if surrounding >= 4 then Empty
                    else Occupied
            let te = pos = (0, 1)
            (pos, newState))
        |> Map.ofSeq

    let rec loopUpdate iterCount seatState =
        if iterCount > 6 then [seatState]
        else
            let newState = updateSeating seatState
            seatState::(loopUpdate (iterCount + 1) newState)

    let finalState = loopUpdate 0 seating
    
    finalState
    |> Seq.map getPrint
    |> Seq.iteri (fun i x ->
        printfn "Iteration: %d" i
        printfn "%s" x)

    ()

let [<Solution("2020", "11", "a")>] partA () =
    let testing = testing() // TODO: This is no longer needed
    let seating = loadData()

    let getSurrounding (seatState: Map<(int * int), SeatState>) (x, y) =
        seq {
            for i in [(x - 1) .. (x + 1)] do
                for j in [(y - 1) .. (y + 1)] do
                    if i <> 0 && j <> 0 then
                        yield (i, j)
        }
        |> Seq.choose (fun x -> Map.tryFind x seatState)
        |> Seq.countBy id
        |> Map.ofSeq

    let getSurroundFunc seatState pos =
        let surround = getSurrounding seatState pos
        (fun x ->
            surround
            |> Map.tryFind x
            |> Option.defaultValue 0)

    let updateSeating (seatState: Map<(int * int), SeatState>) =
        seatState
        |> Map.toSeq
        |> Seq.map (fun (pos, state) ->
            let newState =
                match state with
                | Floor -> Floor
                | Empty ->
                    let surrounding = getSurroundFunc seatState pos Occupied
                    if surrounding = 0 then Occupied
                    else Empty
                | Occupied ->
                    let surrounding = getSurroundFunc seatState pos Occupied
                    if surrounding >= 4 then Empty
                    else Occupied
            (pos, newState))
        |> Map.ofSeq

    let rec loopUpdate iterCount seatState =
        let newState = updateSeating seatState
        printfn "Iterating to: %d" iterCount
        if compareStates seatState newState then seatState
        else loopUpdate (iterCount + 1) newState

    let finalState = loopUpdate 0 seating

    finalState
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.filter (
        function
        | Occupied -> true
        | _ -> false
    )
    |> Seq.length
    |> sprintf "%d"

let [<Solution("2020", "11", "b")>] partB () =
    "hello"