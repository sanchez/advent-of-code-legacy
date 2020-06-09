namespace Sanchez.AOC.Core

type SolutionAttribute(year: string, day: string, part: string) =
    inherit System.Attribute()
    member val Year = year with get
    member val Day = day with get
    member val Part = part with get