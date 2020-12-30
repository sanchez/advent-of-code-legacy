module Sanchez.AOC.Solutions._2020.Day11

open System
open Sanchez.AOC.Core

type SeatState =
    | Occupied
    | Empty
    | Floor
type SeatMap = Map<(int * int), SeatState>

let printState (state: Map<int * int, SeatState>) =
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
                    |> Map.tryFind (y, x)
                    |> Option.map stateChar
                    |> Option.defaultValue '-'
            yield '\n'
    }
    |> String.Concat

let getSurrounding (seatState: SeatMap) (x, y) search =
    let test =
        seq {
            for i in [(x - 1) .. (x + 1)] do
                for j in [(y - 1) .. (y + 1)] do
                    if i <> x || j <> y then
                        yield (i, j)
        }
        |> Seq.toArray
    let itemCounts =
        seq {
            for i in [(x - 1) .. (x + 1)] do
                for j in [(y - 1) .. (y + 1)] do
                    if i <> x || j <> y then
                        yield (i, j)
        }
        |> Seq.choose (fun x -> Map.tryFind x seatState)
        |> Seq.countBy id
        |> Map.ofSeq
    itemCounts
    |> Map.tryFind search
    |> Option.defaultValue 0

let inline addDirToPath (dirX, dirY) (posX, posY) =
    (dirX + posX, dirY + posY)

let rec getIsOccupiedPath (seatState: SeatMap) dir pos =
    let current = Map.tryFind pos seatState

    match current with
    | None -> false
    | Some x ->
        match x with
        | Occupied -> true
        | Empty -> false
        | Floor -> getIsOccupiedPath seatState dir (addDirToPath dir pos)

let updateSeatShort seatState (pos, currentState) =
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

let updateSeatLong seatState (pos, currentState) =
    let surrounding =
        seq {
            for i in -1 .. 1 do
                for j in -1 .. 1 do
                    if i <> 0 || j <> 0 then
                        let dir = (i, j)
                        yield
                            pos
                            |> addDirToPath dir
                            |> getIsOccupiedPath seatState dir
        }
        |> Seq.filter id
        |> Seq.length
    let newState =
        match currentState with
        | Floor -> Floor
        | Empty ->
            if surrounding = 0 then Occupied
            else Empty
        | Occupied ->
            if surrounding >= 5 then Empty
            else Occupied
    (pos, newState)

let updateSeating updator (seatState: SeatMap) =
    seatState
    |> Map.toSeq
    |> Seq.map (updator seatState)
    |> Map.ofSeq

let compareStates (previous: Map<int * int, SeatState>) (next: Map<int * int, SeatState>) =
    let res =
        next
        |> Map.toSeq
        |> Seq.filter (fun (pos, newState) ->
            let oldState = Map.find pos previous
            newState <> oldState)
        |> Seq.toArray
    // printfn "New Resolve: %d" res.Length
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
    """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""
    |> processData

let [<Solution("2020", "11", "a")>] partA () =
    let data = loadData()

    let rec loopState seatState =
        let newState = updateSeating updateSeatShort seatState
        if compareStates seatState newState then newState
        else
            loopState newState

    let stableState = loopState data

    stableState
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.countBy id
    |> Map.ofSeq
    |> Map.tryFind Occupied
    |> Option.defaultValue 0
    |> sprintf "%d"

let [<Solution("2020", "11", "b")>] partB () =
    let data = loadData()

    let rec loopState seatState =
        let newState = updateSeating updateSeatLong seatState
        if compareStates seatState newState then newState
        else
            loopState newState

    let stableState = loopState data

    stableState
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.countBy id
    |> Map.ofSeq
    |> Map.tryFind Occupied
    |> Option.defaultValue 0
    |> sprintf "%d"