module Sanchez.AOC.Solutions._2020.Day16

open System
open Sanchez.AOC.Core

type Range = Range of int * int
type Rule = Rule of string * (Range * Range)
type Ticket = Ticket of int array

let inline loadData () =
    InputLoader.load().Split("\n")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.toArray

let parseData (x: string array) =
    let rules =
        x
        |> Seq.takeWhile (fun x -> x <> "your ticket:")
        |> Seq.map (tryRegex "^([^:]*): (\\d+)-(\\d+) or (\\d+)-(\\d+)$" >> Option.get)
        |> Seq.map (fun x ->
            let r1 = Range (int x.[1], int x.[2])
            let r2 = Range (int x.[3], int x.[4])
            Rule (x.[0], (r1, r2)))
        |> Seq.toList

    let myTicket =
        x
        |> Seq.skipWhile (fun x -> x <> "your ticket:")
        |> Seq.skip 1
        |> Seq.head
        |> (fun x -> x.Split(","))
        |> Seq.map (fun x -> x.Trim())
        |> Seq.map int
        |> Seq.toArray
        |> Ticket

    let otherTickets =
        x
        |> Seq.skipWhile (fun x -> x <> "nearby tickets:")
        |> Seq.skip 1
        |> Seq.map (fun x -> 
            x.Split(",")
            |> Seq.map (fun y -> y.Trim())
            |> Seq.map int
            |> Seq.toArray
            |> Ticket)
        |> Seq.toList

    (rules, myTicket, otherTickets)

let isWithinRange (Range (lower, upper)) (value: int) = (value >= lower) && (value <= upper)
let isValidField (Rule (name, (r1, r2))) (value: int) = (isWithinRange r1 value) || (isWithinRange r2 value)
let findRule rules value =
    rules
    |> Seq.tryFind (fun x -> isValidField x value)

let ticketErrorRate (rules: Rule list) (Ticket ticket) =
    ticket
    |> Seq.filter (findRule rules >> Option.isNone)
    |> Seq.fold (+) 0

let [<Solution("2020", "16", "a")>] partA () =
    let (rules, myTicket, otherTickets) =
        loadData()
        |> parseData

    otherTickets
    |> Seq.map (ticketErrorRate rules)
    |> Seq.reduce (+)
    |> sprintf "%d"

let rec createAllPossibleEntries (a: Rule array) =
    a
    |> Seq.mapi (fun i x ->
        let others = Array.filter ((=) x >> not) a
        let computed = 
            createAllPossibleEntries others
            |> List.map (fun y -> x::y)
        
        computed)
    |> Seq.collect id
    |> Seq.toList

let isValidOrder (rules: Rule list) (Ticket ticket) =
    Seq.zip rules ticket
    |> Seq.map (fun (a, b) -> isValidField a b)
    |> Seq.fold (fun acc x ->
        match (acc, x) with
        | (true, true) -> true
        | _ -> false) true

let isValidCombination (rules: Rule list) (tickets: Ticket list) =
    tickets
    |> Seq.filter (isValidOrder rules >> not)
    |> Seq.length
    |> (fun x -> x = 0)

let [<Solution("2020", "16", "b")>] partB () =
    let (rules, myTicket, otherTickets) =
        loadData()
        |> parseData

    let validTickets =
        otherTickets
        |> Seq.filter (ticketErrorRate rules >> ((=) 0))
        |> Seq.toList

    let allRuleCombinations =
        rules
        |> List.toArray
        |> createAllPossibleEntries
        |> Seq.filter (fun x -> isValidCombination x validTickets)
        |> Seq.toList

    "hello"