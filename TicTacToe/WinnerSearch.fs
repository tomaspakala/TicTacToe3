namespace TicTacToe
module WinnerSearch =
    let tableSize = 3
    let countSize = 2

    let universalCount (moves:(char*string[][])list) f = moves |> Seq.filter (f) 
                                                               |> Seq.length

    let checkRowElement (move:char*string[][]) num xo = ((snd move).[1].[1] = string num) && ((snd move).[2].[1] = xo)

    let rec checkRows moves num xo =
        let check = universalCount moves (fun elem -> checkRowElement elem num xo)
        if check = tableSize then true
        else if num <> countSize then checkRows moves (num + 1) xo
        else false
        
    let checkColumnElement (move:char*string[][]) num xo = ((snd move).[0].[1] = string num) && ((snd move).[2].[1] = xo)

    let rec checkColumns moves num xo =
        let check = universalCount moves (fun elem -> checkColumnElement elem num xo)
        if check = tableSize then true
        else if num <> countSize then checkColumns moves (num + 1) xo
        else false

    let checkDiagnole1Element (move:char*string[][]) xo = ((snd move).[0].[1] = (snd move).[1].[1] && xo = (snd move).[2].[1])

    let checkDiagnole2Element (move:char*string[][]) xo = (int (snd move).[0].[1] + int (snd move).[1].[1]) = countSize && xo = (snd move).[2].[1]

    let winner (moves:(char*string[][])list) xo = 
        async {
            match moves with
            | [] -> return None
            | _  -> if universalCount moves (fun elem -> checkDiagnole1Element elem xo) = tableSize || 
                       universalCount moves (fun elem -> checkDiagnole2Element elem xo) = tableSize ||
                       checkColumns moves 0 xo ||
                       checkRows moves 0 xo
                       then return Some (xo)
                       else return None
        }

    let asyncFindWinner (moves:(char*string[][])list) = 
        [winner (moves) "o"; winner (moves) "x"]
        |> Async.Parallel
        |> Async.RunSynchronously

    let findWinner (moves:(char*string[][])list) = 
        let results = asyncFindWinner moves
        if results.[0] = Some("x") || results.[1] = Some("x") then Some("x")
           elif results.[0] = Some("o") || results.[1] = Some("o") then Some("o")
        else None