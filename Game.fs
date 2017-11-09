module Game

open System

type Roll =
| Simple
| Missed
| Spare

type RollInfo = {
    Points : int;
    Type : Roll
}

type Frame = {
    First : RollInfo;
    Second : RollInfo;
}

let roll throw =
    match throw with
    | '-' -> { Type = Missed; Points = 0 }
    | '/' -> { Type = Spare; Points = 0}
    | _ -> { Type = Simple; Points = throw.ToString() |> int }

let rolls punctuation =
    punctuation 
    |> Seq.toList
    |> Seq.map roll

let frames rolls =
    let chunks = Seq.chunkBySize 2 rolls //pairwise??
    chunks |> Seq.map (fun chunk -> { First = chunk.[0]; Second = chunk.[1]}) |> Seq.toArray

let findFrame frame frames =
    let index = Array.IndexOf(frames, frame)
    
    if index = frames.Length - 1 then 10
    else 
        let nextFrame = frames.[index + 1]
        
        match nextFrame.Second.Type with
        | Spare  -> 10
        | Simple | Missed -> nextFrame.First.Points + 10

let count frames=
    Array.fold (fun state frame -> 
            match frame.First.Type, frame.Second.Type with
            | Simple, Missed | Missed, Simple | Simple, Simple | Missed, Missed
                -> frame.First.Points + frame.Second.Points + state
            | _, Spare | Spare, _ ->  findFrame frame frames) 
            0 frames

let score punctuation =
    punctuation 
    |> rolls
    |> frames
    |> count

//Eliminate de concept of rolls?? 
