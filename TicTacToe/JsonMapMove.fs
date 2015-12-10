namespace TicTacToe
module JsonMapMove =
    let tableSize = 2

    let moveExistation (move:char*string[][]) x y = int (snd move).[0].[1] = x && int (snd move).[1].[1] = y

    let checkMove (moves:(char*string[][])list) x y = Seq.exists(fun elem -> moveExistation elem x y) moves

    let rec getMove (moves:(char*string[][])list) x y =
        if (checkMove moves x y = false) then (x, y)
        else if (x < tableSize) then getMove moves (x + 1) y
        else if (y < tableSize && x = tableSize) then getMove moves 0 (y + 1)
        else if (y < tableSize) then getMove moves x (y + 1)
        else (0, 0)

    let lastMove msg =
        let moves = DecodeJsonMap.decodeJson msg
        let n = moves.Length
        let xy = getMove moves 0 0
        let msg' = msg.[0.. msg.Length - 2]
        msg' + sprintf ", \"%d\": {\"x\": %d, \"y\": %d, \"v\": \"x\"}}" n (fst xy) (snd xy)

    let addMove msg:string =
        match msg with
        | "" -> sprintf "{\"%d\": {\"x\": %d, \"y\": %d, \"v\": \"x\"}}"  0  1  1
        | _  -> lastMove msg