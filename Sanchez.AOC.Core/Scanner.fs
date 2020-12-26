namespace Sanchez.AOC.Core

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

[<AutoOpen>]
module Scanner =
    let check f x =
        if f x then x
        else failwithf "Format Failure: \"%s\"" x
        
    let parseDecimal x = Decimal.Parse(x, System.Globalization.CultureInfo.InvariantCulture)
    
    let parsers =
        dict [
            'd', int >> box
            'b', Boolean.Parse >> box
            'i', int >> box
            's', box
            'u', uint32 >> int >> box
            'x', check (String.forall Char.IsLower) >> ((+) "0x") >> int >> box
            'X', check (String.forall Char.IsUpper) >> ((+) "0x") >> int >> box
            'o', ((+) "0o") >> int >> box
            'e', float >> box
            'E', float >> box
            'f', float >> box
            'F', float >> box
            'g', float >> box
            'G', float >> box
            'M', parseDecimal >> box
            'c', char >> box
        ]
        
    let separators =
        parsers.Keys
        |> Seq.map (fun x -> sprintf "%%%c" x)
        |> Seq.toArray
        
    let rec getFormatters xs =
        match xs with
        | '%'::'%'::xr -> getFormatters xr
        | '%'::x::xr ->
            if parsers.ContainsKey x then x::getFormatters xr
            else failwithf "Unknown Formatter: %%%c" x
        | x::xr -> getFormatters xr
        | [] -> []
        
    let sscanf (pf: PrintfFormat<_, _, _, _, 't>) s : 't =
        let formatString = pf.Value.Replace("%%", "%")
        let constants = formatString.Split(separators, StringSplitOptions.None)
        let regex = Regex("^" + String.Join("(.*?)", constants |> Array.map Regex.Escape) + "$")
        let formatters =
            pf.Value.ToCharArray()
            |> Array.toList
            |> getFormatters
        let groups =
            regex.Match(s).Groups
            |> Seq.cast<Group>
            |> Seq.skip 1
        let matches =
            (groups, formatters)
            ||> Seq.map2 (fun g f -> g.Value |> parsers.[f])
            |> Seq.toArray
            
        if matches.Length = 1 then matches.[0] :?> 't
        else FSharpValue.MakeTuple(matches, typeof<'t>) :?> 't
        
    let trySscanf pf s =
        try
            sscanf pf s
            |> Some
        with
        | _ -> None

    let tryRegex expr s =
        let re = new Regex(expr)
        let res = re.Match(s)

        if res.Success then
            res.Groups
            |> Seq.skip 1
            |> Seq.map (fun x -> x.Value)
            |> Seq.toList
            |> Some
        else None

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then
            m.Groups
            |> Seq.skip 1
            |> Seq.map (fun x -> x.Value)
            |> Seq.toList
            |> Some
        else None