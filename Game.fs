module Game

type RollType =
| Simple
| Spare
| Strike

type Roll = {
    Type:RollType;
    Pins:int
}

let roll throw =
    match throw with
    | '-' -> {Type=Simple;Pins=0}
    | '/' -> {Type=Spare;Pins=10}
    | 'X' -> {Type=Strike;Pins=10}
    | _ -> {Type=Simple;Pins=(throw.ToString() |> int)} 

let calculateSpare iroll (rolls:seq<int * Roll>) = 
    let index, roll = iroll
    
    let previousRoll = Seq.tryItem (index - 1) rolls
    let nextRoll = Seq.tryItem (index + 1) rolls        

    match previousRoll, nextRoll with
    | Some (_,previousRoll), Some (_, nextRoll) -> nextRoll.Pins + roll.Pins - previousRoll.Pins
    | Some (_,previousRoll), _ -> roll.Pins - previousRoll.Pins
    | _ -> 0

let calculateStrike iroll (rolls:seq<int * Roll>) =
    let index, strike = iroll
    let restRolls = rolls
                    |> Seq.filter (fun (i, _) -> i >= index) 
                    |> Seq.map (fun (_, r) -> r)
                    |> Seq.tryTake 3 
                    |> Seq.toList

    if restRolls |> Seq.exists (fun r -> r.Type = Spare ) then
        strike.Pins + 10
    else
        restRolls |> Seq.sumBy (fun r -> r.Pins)

let rolls punctuation =
    punctuation 
    |> Seq.toList
    |> Seq.mapi (fun i throw -> (i, roll throw))

let count rolls =
    Seq.fold (fun accPunctuation iroll -> 
            let _, roll = iroll            
            let points = match roll.Type with
                            | Strike -> calculateStrike iroll rolls
                            | Spare -> calculateSpare iroll rolls
                            | Simple -> roll.Pins                                               
            points + accPunctuation)
            0 rolls

let score punctuation =
    punctuation 
    |> rolls
    |> count