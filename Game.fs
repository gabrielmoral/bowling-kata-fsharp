module Game

open System

type Roll =
| Simple of int
| Missed
| Spare

let spareValue = 10

let roll throw =
    match throw with
    | '-' -> Missed 
    | '/' -> Spare
    | _ -> Simple (throw.ToString() |> int)

let rolls punctuation =
    punctuation 
    |> Seq.toList
    |> Seq.map roll
    |> Seq.chunkBySize 2
    |> Seq.map (fun chunk -> chunk.[0], chunk.[1]) 
    |> Seq.toArray

let calculateSpare frame frames =
    let index = Array.IndexOf(frames, frame)
    
    if index = frames.Length - 1 then spareValue
    else 
        let nextFrame = frames.[index + 1]
        
        match nextFrame with
        | Simple points, Simple _ -> points + spareValue
        | _, _ -> spareValue
       
let count frames=
    Array.fold (fun accPunctuation frame -> 
            match frame with
            | _, Spare | Spare, _ -> (calculateSpare frame frames) + accPunctuation
            | Simple points1, Simple points2 -> points1 + points2 + accPunctuation
            | Simple points, _ | _ , Simple points -> points + accPunctuation
            | _ , _ -> 0)
            0 frames

let score punctuation =
    punctuation 
    |> rolls
    |> count