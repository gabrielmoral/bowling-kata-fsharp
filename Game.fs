module Game

open System

let score (punctuation:string) =
    punctuation 
    |> Seq.toList
    |> Seq.filter Char.IsDigit
    |> Seq.map (string >> int)
    |> Seq.sum
