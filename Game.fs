module Game

open System



type Roll =
| Simple
| Missed

type RollInfo = {
    Points : int;
    Type : Roll
}

let scoreThrow throw =
    match throw with
    | '-' -> 0
    | _ -> (throw.ToString() |> int)

let roll throw =
    let rollType = match throw with
                    | '-' -> Missed
                    | _ -> Simple
    { Points = scoreThrow throw ; Type = rollType}

let filterSimple = fun roll -> match roll.Type with 
                                | Simple _ -> Some roll 
                                | Missed -> None

let rolls punctuation =
    punctuation 
    |> Seq.toList
    |> Seq.map roll

let sum (rolls:seq<RollInfo>) = 
    rolls |> Seq.sumBy (fun t -> t.Points)

let score (punctuation:string) =
    punctuation 
    |> rolls
    |> Seq.choose filterSimple
    |> sum
