namespace TicTacToeTests
module DecodeJsonMapTests =
    open NUnit.Framework
    open FsUnit
    open FsCheck
    open TicTacToe.DecodeJsonMap

    let movesData rep = 
        let strStart = "{\"0\": {\"x\": 2, \"y\": 2, \"v\": \"x\"}" 
        let strEnd = "}"
        let move = ", \"0\": {\"x\": 2, \"y\": 2, \"v\": \"x\"}"
        let makeInt numb = (int numb) |> int
        strStart + String.replicate rep move + strEnd, rep
          
    [<Test>]
    let ``check none move``() = 
        Assert.AreEqual((decodeJson "").Length, 0)

            
    [<Test>]
    let ``check one move``() = 
        let testData = movesData 0
        Assert.AreEqual((decodeJson (fst testData)).Length - 1, snd (testData))

    [<Test>]
    let ``check max moves``() = 
        let testData = movesData 8
        Assert.AreEqual((decodeJson (fst testData)).Length - 1, snd (testData))