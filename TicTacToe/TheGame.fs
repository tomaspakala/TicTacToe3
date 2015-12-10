namespace TicTacToe
module TheGame =
    open System
    open FSharp.Data
    open FSharp.Data.HttpRequestHeaders

    let url = "http://tictactoe.homedir.eu/game/test48/player/1"

    let rec playGame msg = 
        let msgBeforePost = DecodeJsonMap.decodeJson msg
        let winner = WinnerSearch.findWinner (msgBeforePost)
        if winner = None
            then let postMsg = (JsonMapMove.addMove msg)
                 let postResponse = Http.RequestString(url, headers = ["Content-Type", "application/json+map"], body = TextRequest (postMsg))
                 let winner = WinnerSearch.findWinner (DecodeJsonMap.decodeJson postMsg)
                 if winner = None && msgBeforePost.Length <> 8
                    then let getResponse = Http.RequestString(url, headers = ["Accept", "application/json+map"], httpMethod = "GET")
                         playGame (getResponse)
                    else winner
            else winner
     
    let main = printfn "Winner is - %O" (playGame "")