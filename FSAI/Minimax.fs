namespace FSAI

module Minimax =

    type Class1() = 
        member this.X = "F#"

    let boardSize = int 8
    let empty = byte  0
    let white = byte  1
    let black = byte  2
    let valid = byte  3
    let tie =  byte 4

    //Directions in the game
    let dirs = [(-1,1) ; (0,1) ; (1,1) ; (-1,0) ; (1,0) ; (1-,1-) ; (0,-1) ; (1,-1)] //All 8 possible directions of the game

    //Function check if it inside the board 8x8
    let IsOnBoard x y = 
        0 <= x && x <= 7 && 0 <= y && y <= 7; //See if its inside board of 8x8 tiles

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
    
    let GetScore (board : byte[,]) (tile : byte) = 
        Seq.cast<byte> board  //Set start value
        |> Seq.filter (fun cell -> cell = tile)  |> Seq.length //Filter each cell in the board


    let getWinner (board : byte[,]) = 
        let whiteScore = getScore board white //Get score for white
        let blackScore = getScore board black //Get score for black
        if whiteScore = 0 || blackScore = 0 || whiteScore + blackScore = 64 || 
            list.length(getValidMoves board black) + list.length(getValidMoves board white) = 0 then // If statement to get stats for each player
            if blackScore > whiteScore then black //If blackScore greater then whiteScore then black return as winner
            elif whiteScore > blackScore then white //If whiteScore greater then blackScore then white return as winner
            else then tie //If whiteScore is equal blackScore then return tie 
        else // Error? or empty
    
    let otherTile tile = 
        if tile = black then white //check if player black then return player white
        elif tile = white then black //Opposit, check if player white then return player black
        else //error if not if npt value 1 or 2

    let GetFlippedPieces //Implement
    let GetValidMoves //Implement
    let MakeMove //Implement
    let CountCorners //Implement


