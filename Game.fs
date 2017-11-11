module Game

open System

type Roll =
| Simple of int
| Missed
| Spare

type Frame = {
    First : Roll;
    Second : Roll;
}

let roll throw =
    match throw with
    | '-' -> Missed 
    | '/' -> Spare
    | _ -> Simple (throw.ToString() |> int)

let rolls punctuation =
    punctuation 
    |> Seq.toList
    |> Seq.map roll

let frames rolls =
    let chunks = Seq.chunkBySize 2 rolls //pairwise??
    chunks 
    |> Seq.map (fun chunk -> { First = chunk.[0]; Second = chunk.[1]}) 
    |> Seq.toArray

let calculateSpare frame frames =
    let index = Array.IndexOf(frames, frame)
    
    if index = frames.Length - 1 then 10
    else 
        let nextFrame = frames.[index + 1]
        
        match nextFrame.First, nextFrame.Second with
        | Simple points, Simple _ -> points + 10
        | _, _ -> 10
       
let count frames=
    Array.fold (fun accPunctuation frame -> 
            match frame.First, frame.Second with
            | _, Spare | Spare, _ -> (calculateSpare frame frames) + accPunctuation
            | Simple points1, Simple points2 -> points1 + points2 + accPunctuation
            | Simple points, _ | _ , Simple points -> points + accPunctuation
            | _ , _ -> 0)
            0 frames

let score punctuation =
    punctuation 
    |> rolls
    |> frames
    |> count

//Eliminate de concept of rolls?? 
