namespace FSAI

module Minimax =
    // let printHello message = 
    //     $"Hello, world {message}"

    type Class1() = 
        member this.X = "F#"

    let Evaluation board:byte[,] = //Evaluation function take a board

        let blackScore = getScore black //Get score for player black
        let whiteScore = getScore white //Get score for player white
        let blackMobility = GetValidMoves board black //Get player blacks valid moves in the game
        let whiteMobility = GetValidMoves board white //Get player blacks valid moves in the game

        if blackScore = 0 then -200000 //if black score = 0 then return -200000
        elif whiteScore = 0 then 200000 //if whites score = 0 then return 200000
        else //If none of bellow then nothing

        if blackScore + whiteScore == 64 || blackMobility + whiteMobility == 0 then
            if Black < whiteScore then
                 -100000 - whiteScore + blackScore
            elif blackScore > whiteScore then
                100000 + blackScore - whiteScore
            else
                0
        else then 
            
            if blackScore + whiteScore > 55 then blackScore - whiteScore
            else then 
                let eval = blackScore - whiteScore //Count blacks score - whites score
                let eval1 = eval + (blackMobility - whiteMobility) * 10 //take value from eval and multiple by 10
                let eval2 = eval1 + ((CountCorners board black) - (CountCorners board white)) * 100 //take value2 from eval and multiple by 100
                
                eval2 //Return value from eval2
    
    let GetScore board:byte[,] tile = 
        let score = 0 //Set start value for score
        for cell in board do  //Foreach cell in the board
            if cell = tile then score++ //If cell is tile then add one to score
        score // return the value of score
    

    let Action "state" = //Implement
    let getScore "player" = //Implement
    let getValidMoves "board & player" = //Implement
    let Result "state action" = //Implement
