#light
namespace TicTacToe
module Main =

    [<EntryPoint>]
    let main args =
        let game = TheGame.main
        let a = System.Console.ReadLine()
        0