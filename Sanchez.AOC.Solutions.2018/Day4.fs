module Sanchez.AOC.Solutions._2018.Day4

open System
open Sanchez.AOC.Core

type LogType =
    | Starting of int
    | Sleeping
    | Waking
    
let getLogType m =
    if m = "wakes up" then Waking
    elif m = "falls asleep" then Sleeping
    else
        sscanf "Guard #%d begins shift" m
        |> Starting
        
type GuardState =
    | Awake
    | Sleep
type Guard = Guard of int*(GuardState array)
module Guard =
    let create i =
        (i, Array.create 60 Awake)
        |> Guard
    let waking (stamp: DateTime) (Guard (i, s)) =
        seq { stamp.Minute .. 59 }
        |> Seq.iter (fun x -> s.[x] <- Awake)
        (i, s) |> Guard
    let sleeping (stamp: DateTime) (Guard (i, s)) =
        seq { stamp.Minute .. 59 }
        |> Seq.iter (fun x -> s.[x] <- Sleep)
        (i, s) |> Guard
    let minutesAsleep (Guard (i, s)) =
        s
        |> Seq.filter ((=) Sleep)
        |> Seq.length
    let asleepMinutes (Guard (i, s)) =
        s
        |> Seq.mapi (fun i x -> (i, x))
        |> Seq.filter (snd >> ((=) Sleep))
        |> Seq.map fst
        |> Seq.toArray

let [<Solution("2018", "4", "a")>] partA () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.choose (trySscanf "[%d-%d-%d %d:%d] %s")
        |> Seq.map (fun (year, month, day, hour, minute, message) -> (new DateTime(year, month, day, hour, minute, 0), getLogType message))
        |> Seq.sortBy fst
        |> Seq.fold (fun (currGuard, allGuards) (stamp, log) ->
            match log with
            | Starting i ->
                (Guard.create i, currGuard::allGuards)
            | Waking -> (Guard.waking stamp currGuard, allGuards)
            | Sleeping -> (Guard.sleeping stamp currGuard, allGuards)) (Guard.create -1, [])
        |> (fun (a, b) -> a::b)
        |> Seq.groupBy (fun (Guard (i, _)) -> i)
        |> Seq.toArray
        
    let (mostAsleepGuard, _) =
        input
        |> Seq.map (fun (i, guards) ->
            let timeAsleep =
                guards
                |> Seq.map Guard.minutesAsleep
                |> Seq.sortDescending
                |> Seq.head
            (i, timeAsleep))
        |> Seq.sortByDescending snd
        |> Seq.head
        
    let guardShifts =
        input
        |> Array.find (fst >> ((=) mostAsleepGuard))
        |> snd
        |> Seq.map (fun (Guard (_, s)) ->
            s
            |> Seq.mapi (fun i x -> (i, x))
            |> Seq.filter (snd >> ((=) Sleep))
            |> Seq.map fst)
        |> Seq.collect id
        |> Seq.groupBy id
        |> Seq.sortByDescending (snd >> Seq.length)
        |> Seq.map fst
        |> Seq.head
        
    let result = guardShifts * mostAsleepGuard
        
    result
    |> string
    
let [<Solution("2018", "4", "b")>] partB () =
    let input =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.choose (trySscanf "[%d-%d-%d %d:%d] %s")
        |> Seq.map (fun (year, month, day, hour, minute, message) -> (new DateTime(year, month, day, hour, minute, 0), getLogType message))
        |> Seq.sortBy fst
        |> Seq.fold (fun (currGuard, allGuards) (stamp, log) ->
            match log with
            | Starting i ->
                (Guard.create i, currGuard::allGuards)
            | Waking -> (Guard.waking stamp currGuard, allGuards)
            | Sleeping -> (Guard.sleeping stamp currGuard, allGuards)) (Guard.create -1, [])
        |> (fun (a, b) -> a::b)
        |> Seq.groupBy (fun (Guard (i, _)) -> i)
        |> Seq.toArray
        
    let (guard, minute, _) =
        input
        |> Seq.choose (fun (i, shifts) ->
            shifts
            |> Seq.map Guard.asleepMinutes
            |> Seq.collect id
            |> Seq.groupBy id
            |> Seq.sortByDescending (snd >> Seq.length)
            |> Seq.tryHead
            |> Option.map (fun (sleepMinute, c) -> (i, sleepMinute, c |> Seq.length)))
        |> Seq.sortByDescending (fun (_, _, a) -> a)
        |> Seq.head
        
    let res = guard * minute
    
    res |> string