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

    let dirs =[
        (-1,1); (0,1); (1,1);
        (-1,1);         (1,0);
        (-1,-1); (0,-1);(1,-1);
        ]

    //Function check if it inside the board 8x8
    let IsOnBoard x y = 
        0 <= x && x <= 7 && 0 <= y && y <= 7 //See if its inside board of 8x8 tiles


    let GetFlippedPieces (board : byte[,]) (move : (int * int)) (tile : byte) = 
        let randomList = [(1, 2); (2, 3); (4, 4)] //placeholder
        randomList //placeholder

    let OtherTile tile = 
        if tile = black then white //check if player black then return player white
        elif tile = white then black //Opposit, check if player white then return player black
        else byte -1//error if not if npt value 1 or 2

    let GetScore (board : byte[,]) (tile : byte) =
        Seq.cast<byte> board  //Set start value
        |> Seq.filter (fun cell -> cell = tile)  |> Seq.length //Filter each cell in the board

    let GetValidMoves (board : byte[,]) (tile : byte) = //KALLA PÅ DENNA IFRÅN GAME:CS//
        let randomList = [(1, 2); (2, 3); (4, 4)] //placeholder
        randomList //placeholder
       //or x in 0..7 do
       //  for y in 0..7 
       //       if board.[x,y] = empty then 
        //          let doneMove = 0
                    

    let CountCorners (board : byte[,]) (tile : byte) =
        let validCorners = [(board.[0,0]);(board.[0,7]);(board.[7,0]);(board.[7,7])] //Create a list of valid corners
        let mutable myCorners = [] //Create an empty list where we are going to append all the corners we are holding
        for corner in validCorners do // go over all corners in the first list we created
            if corner = tile then myCorners <- [corner] |> List.append myCorners //Check if the corner in the loop is occupied by our tile and if it is add it to myCorners list
        myCorners.Length //Return  the length of the myCorners list to get all the inhabited corners of the tile type we are checking

    let Evaluation (board : byte[,]) = //Evaluation function take a board

        let blackScore = GetScore board black //Get score for player black
        let whiteScore = GetScore board white //Get score for player white
        let blackMobility = GetValidMoves board black //Get player blacks valid moves in the game //KALLA PÅ DENNA IFRÅN GAME:CS//
        let whiteMobility = GetValidMoves board white //Get player blacks valid moves in the game //KALLA PÅ DENNA IFRÅN GAME:CS//

        if blackScore = 0 then -200000 //if black score = 0 then return -200000
        elif whiteScore = 0 then 200000 //if whites score = 0 then return 200000
     // else //If none of bellow then nothing

        if blackScore + whiteScore = 64 || blackMobility.Length + whiteMobility.Length = 0 then
            if blackScore < whiteScore then
                -100000 - whiteScore + blackScore
            elif blackScore > whiteScore then
                100000 + blackScore - whiteScore
            else
                0
        else
           
           if blackScore + whiteScore > 55 then blackScore - whiteScore
           else 
               let eval = blackScore - whiteScore //Count blacks score - whites score
               let eval1 = eval + (blackMobility.Length - whiteMobility.Length) * 10 //take value from eval and multiple by 10
               let eval2 = eval1 + ((CountCorners board black) - (CountCorners board white)) * 100 //take value2 from eval and multiple by 100
               eval2 //Return value from eval2
   
    let MakeMove (board : byte[,]) (move : (int * int)) (tile : byte) =
        let flippedPieces = GetFlippedPieces board move tile //Runs the getflippedpieces function
        for flippedPiece in flippedPieces do //for every flipped piece
            board.[fst flippedPiece, snd flippedPiece] <- tile //change the existing tile on the board the the new tile
        if not(flippedPieces.IsEmpty) then //If any flipped piece was missed
            board.[fst move, snd move] <- tile //Change the tile
        

    let GetWinner (board : byte[,]) = 
       let whiteScore = GetScore board white //Get score for white
       let blackScore = GetScore board black //Get score for black
       if whiteScore = 0 || blackScore = 0 || whiteScore + blackScore = 64 || 
           List.length(GetValidMoves board black) + List.length(GetValidMoves board white) = 0 then // If statement to get stats for each player
           if blackScore > whiteScore then black //If blackScore greater then whiteScore then black return as winner
           elif whiteScore > blackScore then white //If whiteScore greater then blackScore then white return as winner
           else tie //If whiteScore is equal blackScore then return tie 
       else byte -1// Error? or empty
   
    let MinimaxAlphaBeta (board : byte[,]) (depth : int) (a : int) (b : int) (tile : byte) (isMaxPlayer : bool) =
        if depth = 0 || GetWinner board <> empty then // Check if depth = 0 or if GetWinner is empty
            Evaluation board //If so then return Evaluation board
        
        let bestScore = 0 
        if isMaxPlayer then bestScore = System.Int32.MinValue  //If isMaxPlayer then set bestCore to MinValue
        else bestScore = System.Int32.MaxValue //If not isMaxPlayer then set bestCore to maxValue

        let validMoves = GetValidMoves board tile
        if validMoves.Length > 0 then //Check if validMoves is greater then 0
            for move in validMoves do //For every move in validMoves then do
                let childBoard = board.Clone() //Get board and set as a childboard
                MakeMove childBoard move tile //Add childboard, move and tile to makeMove function
                let nodeScore = MinimaxAlphaBeta childBoard (depth - 1) a b  (otherTile tile) (not isMaxPlayer)
                if isMaxPlayer then //Check if its max player
                    bestScore = max bestScore nodeScore //Max value of bestScore and nodeScore = bestScore
                    a = max bestScore a// max of bestScore and a = a
                else 
                    bestScore = min bestScore nodeScore //Min value of bestScore and nodeScore = bestScore
                    b = min bestScore b // min value of bestScore and b = b
                if b <= a then -1 //Add break here ? //If a is equal or greater then b, then return error/break

        else MinimaxAlphaBeta board depth a b (OtherTile tile) (not isMaxPlayer) //If validMoves is not greater then 0 then return minimaxalphabeta function
        bestScore //Then return bestScore





