module Sanchez.AOC.Solutions._2020.Day14

open System
open Sanchez.AOC.Core

type Instruction =
    | Mask of int64 * int64
    | Memory of int64 * int64

let parseMask (s: string) =
    let reversed = 
        s
        |> Seq.rev
        |> Seq.mapi (fun i x -> (i, x))
        |> Seq.toArray

    let orMask =
        reversed
        |> Seq.filter (snd >> ((=) '1'))
        |> Seq.map (fun (x, _) -> pown 2L x)
        |> Seq.reduce (+)

    let andMask =
        reversed
        |> Seq.filter (snd >> ((=) '0') >> not)
        |> Seq.map (fun (x, _) -> pown 2L x)
        |> Seq.reduce (+)

    (orMask, andMask)

let toInstruction =
    function
    | Regex "^mask = (.*)$" x -> x.[0] |> parseMask |> Mask
    | Regex "^mem\\[([^\\]]+)\\] = (.*)$" x -> Memory (int64 x.[0], int64 x.[1])
    | x -> failwithf "Failed to parse: %s" x

let inline loadData () =
    InputLoader.load().Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.map toInstruction
    |> Seq.toArray

let executeInstruction ((curOrMask, curAndMask), currentMemory) =
    function
    | Mask (orMask, andMask) -> ((orMask, andMask), currentMemory)
    | Memory (addr, value) ->
        let maskedVal = (value ||| curOrMask) &&& curAndMask
        let newMemory = currentMemory |> Map.add addr maskedVal
        ((curOrMask, curAndMask), newMemory)

let [<Solution("2020", "14", "a")>] partA () =
    let data =
        loadData()

    let (finalMask, finalMemory) =
        data
        |> Seq.fold executeInstruction ((0L, 0L), Map.empty)

    finalMemory
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.reduce (+)
    |> sprintf "%d"

type MaskValue =
    | MaskOne
    | MaskZero
    | MaskForceZero
    | MaskUnknown

type Register = Register of MaskValue array

type NewInstruction =
    | NewMask of Register
    | NewMemory of int64 * int64

let parseRegister (x: string) =
    x
    |> Seq.map (
        function
        | '1' -> MaskOne
        | '0' -> MaskZero
        | 'X' -> MaskUnknown
        | x -> failwithf "Unsupported character: %c" x)
    |> Seq.toArray
    |> Register

let toNewInstruction =
    function
    | Regex "^mask = (.*)$" x -> x.[0] |> parseRegister |> NewMask
    | Regex "^mem\\[([^\\]]+)\\] = (.*)$" x -> NewMemory (int64 x.[0], int64 x.[1])
    | x -> failwithf "Failed to parse: %s" x

let arraySet i value x =
    let local = Array.copy x
    Array.set local i value
    local

let rec allPossibleEntries (Register r) =
    match Array.tryFindIndex ((=) MaskUnknown) r with
    | Some x ->
        let refinedOne = r |> arraySet x MaskOne |> Register
        let refinedZero = r |> arraySet x MaskForceZero |> Register
        [ allPossibleEntries refinedOne; allPossibleEntries refinedZero; ]
        |> Seq.collect id
        |> Seq.toList
    | None -> [Register r]

let registerOrMask (Register r) =
    let reversed =
        r
        |> Seq.rev
        |> Seq.mapi (fun i x -> (i, x))
        |> Seq.toArray

    let orMask =
        reversed
        |> Seq.filter (snd >> ((=) MaskOne))
        |> Seq.map (fst >> pown 2L)
        |> Seq.reduce (+)

    let andMask =
        reversed
        |> Seq.filter (snd >> ((=) MaskForceZero) >> not)
        |> Seq.map (fst >> pown 2L)
        |> Seq.reduce (+)

    (orMask, andMask)

let applyMask register =
    let resolvedMasks =
        register
        |> allPossibleEntries
        |> Seq.map registerOrMask
        |> Seq.toList

    (fun x ->
        resolvedMasks
        |> List.map (fun (orMask, andMask) -> (x ||| orMask) &&& andMask))

let inline loadVer2Data () =
    InputLoader.load().Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.map toNewInstruction
    |> Seq.toArray

let executeNewInstruction (curMaskFunc: int64 -> int64 list, currentMemory) =
    function
    | NewMask r -> (applyMask r, currentMemory)
    | NewMemory (addr, value) ->
        let mem = List.fold (fun acc x -> Map.add x value acc) currentMemory (curMaskFunc addr)
        (curMaskFunc, mem)

let [<Solution("2020", "14", "b")>] partB () =
    let data =
        loadVer2Data()

    let (finalMask, finalMemory) =
        data
        |> Seq.fold executeNewInstruction ((fun x -> List.empty), Map.empty)

    finalMemory
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.reduce (+)
    |> sprintf "%d"