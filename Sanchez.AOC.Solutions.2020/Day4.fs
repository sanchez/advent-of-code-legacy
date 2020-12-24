module Sanchez.AOC.Solutions._2020.Day4

open System
open Sanchez.AOC.Core

let isTrue a b =
    match (a, b) with
    | (true, true) -> true
    | _ -> false

type Passport =
    {
        BirthYear: string option // byr
        IssueYear: string option // iyr
        ExpirationYear: string option // eyr
        Height: string option // hgt
        HairColor: string option // hcl
        EyeColor: string option // ecl
        PassportId: string option // pid
        CountryId: string option // cid
    }

let inline loadData () =
    let (currentData, history) =
        InputLoader.load().Split("\n")
        |> Seq.map (fun x -> x.Trim())
        |> Seq.fold (fun (currentData, history) x ->
            if x.Length = 0 then
                ([], currentData::history)
            else (x::currentData, history)) ([], [])

    currentData::history
    |> Seq.filter (fun x -> x.Length <> 0)
    |> Seq.map (fun x -> String.Join(" ", x))
    |> Seq.map (fun x -> x.Split(' '))
    |> Seq.map (
        Seq.map (fun x ->
            let kv = x.Split(':')
            (kv.[0], kv.[1]))
        >> Map.ofSeq)
    |> Seq.map (fun x ->
        {
            Passport.BirthYear = x |> Map.tryFind "byr"
            IssueYear = x |> Map.tryFind "iyr"
            ExpirationYear = x |> Map.tryFind "eyr"
            Height = x |> Map.tryFind "hgt"
            HairColor = x |> Map.tryFind "hcl"
            EyeColor = x |> Map.tryFind "ecl"
            PassportId = x |> Map.tryFind "pid"
            CountryId = x |> Map.tryFind "cid"
        })
    |> Seq.toArray

let [<Solution("2020", "4", "a")>] partA () =
    let input =
        loadData()
        |> Seq.map (fun x -> [ x.BirthYear; x.IssueYear; x.ExpirationYear; x.Height; x.HairColor; x.EyeColor; x.PassportId])
        |> Seq.map (Seq.map Option.isSome >> Seq.reduce isTrue)
        |> Seq.filter id

    let count = input |> Seq.length

    sprintf "%d" count

let validNumberRange lower upper =
    trySscanf "%d"
    >> Option.map (
        function
        | i when i >= lower && i <= upper -> true
        | _ -> false)
    >> Option.defaultValue false

let validBirthYear = validNumberRange 1920 2002
let validIssueYear = validNumberRange 2010 2020
let validExpirationYear = validNumberRange 2020 2030

let validHeight input =
    let cm =
        trySscanf "%dcm" input
        |> Option.map (
            function
            | i when i >= 150 && i <= 193 -> true
            | _ -> false)
            |> Option.defaultValue false

    let inches =
        trySscanf "%din" input
        |> Option.map (
            function
            | i when i >= 59 && i <= 76 -> true
            | _ -> false)
        |> Option.defaultValue false

    match (cm, inches) with
    | (true, _) | (_, true) -> true
    | _ -> false

let validHairColor (input: string) =
    if input.StartsWith("#") then
        input.[1..]
        |> Seq.map (
            function
            | c when c >= '0' && c <= '9' -> true
            | c when c >= 'a' && c <= 'f' -> true
            | _ -> false)
        |> Seq.fold isTrue true
    else false

let validEyeColor =
    function
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
    | _ -> false

let validPassportId (input: string) =
    if input.Length = 9 then
        trySscanf "%d" input
        |> Option.map (fun _ -> true)
        |> Option.defaultValue false
    else false

let validInput f (input: string option) =
    Option.map f input
    |> Option.defaultValue false

let [<Solution("2020", "4", "b")>] partB () =
    let input =
        loadData()
        |> Seq.map (fun x -> [
            validInput validBirthYear x.BirthYear
            validInput validIssueYear x.IssueYear
            validInput validExpirationYear x.ExpirationYear
            validInput validHeight x.Height
            validInput validHairColor x.HairColor
            validInput validEyeColor x.EyeColor
            validInput validEyeColor x.EyeColor
            validInput validPassportId x.PassportId
        ])
        |> Seq.map (Seq.reduce isTrue)
    
    input
    |> Seq.filter id
    |> Seq.length
    |> sprintf "%d"