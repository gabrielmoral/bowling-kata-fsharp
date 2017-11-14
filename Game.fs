module Game

open System
let spareValue = 10 // include this guy as a part of spare??

type Roll =
| Simple of int
| Missed
| Spare
| Strike of int

type Frame = {
    First:Roll;
    Second:Roll;
    Index:int
}

let roll throw =
    match throw with
    | '-' -> Missed 
    | '/' -> Spare
    | 'X' -> Strike 10
    | _ -> Simple (throw.ToString() |> int)

let rolls punctuation =
    punctuation 
    |> Seq.toList
    |> Seq.map roll
    |> Seq.chunkBySize 2
    |> Seq.mapi (fun i chunk -> {Index=i; First=chunk.[0]; Second = chunk.[1]}) 
    |> Seq.toArray

let calculateSpare frame (frames:Frame[]) =    
    if frame.Index = frames.Length - 1 then spareValue
    else 
        let nextRoll = frames.[frame.Index + 1].First
        
        match nextRoll with
        | Simple points -> points + spareValue
        | _ -> spareValue

let calculateStrike frame (frames:Frame[]) =
    if frame.Index = frames.Length - 1 then 10
    else
       let firstRoll = frame.Second
       let secondRoll = frames.[frame.Index + 1].First

       match firstRoll, secondRoll with
       | Simple points1, Simple points2 -> 10 + points1 + points2
       | _ -> 10

let count frames =
    Array.fold (fun accPunctuation frame ->             
            let framePoints = match frame.First, frame.Second with
                                | _, Spare -> calculateSpare frame frames
                                | Simple points1, Simple points2 -> points1 + points2
                                | Strike _, _ -> calculateStrike frame frames
                                | Simple points, _ | _ , Simple points -> points
                                | _ , _ -> 0
            framePoints + accPunctuation)
            0 frames

let score punctuation =
    punctuation 
    |> rolls
    |> count