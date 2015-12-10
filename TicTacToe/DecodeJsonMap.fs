namespace TicTacToe
module DecodeJsonMap =
    let substr (s:string) i j = s.[i..j]

    let index (msg:string) = msg.IndexOf(msg)

    let rec removeSymbols (msg:string, fromStr:string, toStr:string) = 
        if msg.Contains(fromStr) then
            removeSymbols (msg.Replace(fromStr, toStr), fromStr, toStr)
        else
            msg

    let removeIndex (msg:string) = msg.[2..(String.length msg - 1)]

    let splitColon (msg:string) = msg.Split[|':'|]

    let splitComma (msg:string) = msg.Split[|','|]

    let decode2 (msg:string, moves) = 
        let moveIndex = msg.Chars(0)
        let msg = removeIndex msg
        let commaSeparated = splitComma msg
        let colonSeparated = commaSeparated |> Array.map(fun x -> splitColon x)
        (moveIndex, colonSeparated) :: []

    let preDecode (msg:string) = removeSymbols(removeSymbols(removeSymbols(removeSymbols(msg,"\"","")," ",""),"{",""),"}","")

    let rec decode1 (msg:string, moves) =
        if msg = "" then moves
        else decode1 (msg.[14..(String.length msg - 1)], moves @ decode2(msg.[0..12], moves))

    let decodeJson msg =
        match msg with
        | "" -> []
        | _ ->  decode1 (preDecode msg, [])