module Sanchez.AOC.Core.InputLoader

open System.Diagnostics
open System.IO
open HttpFs.Client
open Hopac

let private inputFolderLocation = "inputs/"

let private loadSession () =
    async {
        let fileName = "inputs/sessionKey.txt"
        let dir = Path.GetDirectoryName fileName

        if dir |> Directory.Exists |> not then
            dir |> Directory.CreateDirectory |> ignore

        return!
            match fileName |> File.Exists with
            | true ->
                fileName
                |> File.ReadAllTextAsync
                |> Async.AwaitTask
            | false ->
                async {
                    do printf "Please enter session key: "
                    let key = System.Console.ReadLine()

                    do!
                        File.WriteAllTextAsync(fileName, key)
                        |> Async.AwaitTask

                    return key
                }
    }

let private loadFromWebSite (year: string) (day: string) =
    async {
        let url = sprintf "https://adventofcode.com/%s/day/%s/input" year day

        let! sessionKey = loadSession()

        let! res =
            url
            |> Request.createUrl Get
            |> Request.cookie (Cookie.create("session", sessionKey))
            |> getResponse
            |> Job.toAsync

        if res.statusCode <> 200 then
            failwithf "Failed to download input: %s" url

        let! input =
            res
            |> Response.readBodyAsString
            |> Job.toAsync

        return input
    }

let private loadFromFile (year: string) (day: string) =
    async {
        let fileName = sprintf "inputs/%s-%s-input.txt" year day
        let dir = Path.GetDirectoryName fileName

        if dir |> Directory.Exists |> not then
            dir |> Directory.CreateDirectory |> ignore

        return!
            match fileName |> File.Exists with
            | false ->
                Async.result None
            | true ->
                fileName
                |> File.ReadAllTextAsync
                |> Async.AwaitTask
                |> Async.map Some
    }

let private saveToFile year day data =
    async {
        let fileName = sprintf "input/%s-%s-input.txt" year day

        let dir = Path.GetDirectoryName fileName

        if dir |> Directory.Exists |> not then
            dir |> Directory.CreateDirectory |> ignore

        do!
            File.WriteAllTextAsync(fileName, data)
            |> Async.AwaitTask

        return ()
    }

let private loadAsync year day =
    async {
        let! file = loadFromFile year day

        let! remapped =
            match file with
            | Some x -> Async.result x
            | None ->
                async {
                    let! web = loadFromWebSite year day
                    do! saveToFile year day web

                    return web
                }

        return remapped
    }


let load () =
    let stackFrame = new StackFrame(1)
    let method = stackFrame.GetMethod()
    let solutionInfo =
        method.CustomAttributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<SolutionAttribute>)
        |> Option.defaultWith (fun () -> failwith "Failed to find solution information")
        
    let loadedInfo =
        solutionInfo.Constructor.Invoke(solutionInfo.ConstructorArguments |> Seq.map (fun x -> x.Value) |> Seq.toArray)
        :?> SolutionAttribute

    let input =
        loadAsync loadedInfo.Year loadedInfo.Day
        |> Async.RunSynchronously

    input