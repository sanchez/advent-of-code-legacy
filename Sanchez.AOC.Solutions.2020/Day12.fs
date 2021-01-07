module Sanchez.AOC.Solutions._2020.Day12

open System
open Sanchez.AOC.Core

type Movement =
    | North of int
    | South of int
    | East of int
    | West of int
    | Left of int
    | Right of int
    | Forward of int

type ShipDir =
    | DirNorth
    | DirSouth
    | DirEast
    | DirWest

let convertToMovement =
    (fun x -> tryRegex "^(.)(\d*)$" x)
    >> Option.get
    >> function
        | [a; b] -> (a |> char, b |> int)
        | x -> failwithf "Yeah nah: %A" x
    >> function
        | ('N', x) -> North x
        | ('S', x) -> South x
        | ('E', x) -> East x
        | ('W', x) -> West x
        | ('L', x) -> Left x
        | ('R', x) -> Right x
        | ('F', x) -> Forward x
        | x -> failwithf "Failed for: %A" x

let inline loadData () =
    InputLoader.load().Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.map convertToMovement
    |> Seq.toList

let loadTestData () =
    """F10
N3
F7
R90
F11""".Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.map convertToMovement
    |> Seq.toList

let turnLeft =
    function
    | DirEast -> DirNorth
    | DirNorth -> DirWest
    | DirWest -> DirSouth
    | DirSouth -> DirEast
let turnRight =
    function
    | DirEast -> DirSouth
    | DirSouth -> DirWest
    | DirWest -> DirNorth
    | DirNorth -> DirEast

let handleDir turningFunc =
    function
    | 90 -> turningFunc
    | 180 -> turningFunc >> turningFunc
    | 270 -> turningFunc >> turningFunc >> turningFunc
    | x -> failwithf "Unsupported angle: %d" x

let makeMovement (dir, (posX, posY)) action =
    match action with
    | North x -> (dir, (posX, posY + x))
    | South x -> (dir, (posX, posY - x))
    | East x -> (dir, (posX + x, posY))
    | West x -> (dir, (posX - x, posY))
    | Left x -> (dir |> handleDir turnLeft x, (posX, posY))
    | Right x -> (dir |> handleDir turnRight x, (posX, posY))
    | Forward x ->
        match dir with
        | DirEast -> (dir, (posX + x, posY))
        | DirSouth -> (dir, (posX, posY - x))
        | DirWest -> (dir, (posX - x, posY))
        | DirNorth -> (dir, (posX, posY + x))

let manhattanDistance (posX: int, posY: int) =
    Math.Abs(posX) + Math.Abs(posY)

let [<Solution("2020", "12", "a")>] partA () =
    let data =
        loadData()

    let startingPosition = (0, 0)
    let startingDir = DirEast

    let (finalDir, finalPos) =
        data
        |> Seq.fold (fun (dir, pos) x -> 
            makeMovement (dir, pos) x) (startingDir, startingPosition)

    let distance = manhattanDistance finalPos

    distance
    |> sprintf "%d"

let rotateLeft (x, y) = (-y, x)
let rotateRight (x, y) = (y, -x)

let makeWaypointMovement ((wayX, wayY), (posX, posY)) action =
    match action with
    | North x -> ((wayX, wayY + x), (posX, posY))
    | South x -> ((wayX, wayY - x), (posX, posY))
    | East x -> ((wayX + x, wayY), (posX, posY))
    | West x -> ((wayX - x, wayY), (posX, posY))
    | Left x -> ((wayX, wayY) |> handleDir rotateLeft x, (posX, posY))
    | Right x -> ((wayX, wayY) |> handleDir rotateRight x, (posX, posY))
    | Forward x -> ((wayX, wayY), (posX + (wayX * x), posY + (wayY * x)))

let [<Solution("2020", "12", "b")>] partB () =
    let data =
        loadData()

    let startingPosition = (0, 0)
    let startingWaypoint = (10, 1)

    let (finalWay, finalPos) =
        data
        |> Seq.fold (fun (way, pos) x ->
            makeWaypointMovement (way, pos) x) (startingWaypoint, startingPosition)

    let distance = manhattanDistance finalPos

    distance
    |> sprintf "%d"