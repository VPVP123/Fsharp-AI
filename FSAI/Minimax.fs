
namespace FSAI

module Minimax =

    let boardSize = int 8
    let empty = byte  0
    let white = byte  1
    let black = byte  2
    let valid = byte  3
    let tie =  byte 4


    let dirs =[ //All possible directions for player tile
        (-1,1); (0,1); (1,1);
        (-1,1);         (1,0);
        (-1,-1); (0,-1);(1,-1);
        ]

    //Function check if it inside the board 8x8
    let IsOnBoard x y = 
        0 <= x && x <= 7 && 0 <= y && y <= 7 //See if its inside board of 8x8 tiles

    //Function OtherTile check tail color and return opposit
    let OtherTile tile = 
        if tile = black then white //check if player black then return player white
        elif tile = white then black //Opposit, check if player white then return player black
        else byte -1//error if not if npt value 1 or 2

    //Function GetScore get score for tile
    let GetScore (board : byte[,]) (tile : byte) =
        Seq.cast<byte> board  //Set start value
        |> Seq.filter (fun cell -> cell = tile)  |> Seq.length //Filter each cell in the board
                   
    //Function CountCorners count corner of the board of 8x8
    let CountCorners (board : byte[,]) (tile : byte) =
        let validCorners = [(board.[0,0]);(board.[0,7]);(board.[7,0]);(board.[7,7])] //Create a list of valid corners
        let mutable myCorners = [] //Create an empty list where we are going to append all the corners we are holding
        for corner in validCorners do // go over all corners in the first list we created
            if corner = tile then myCorners <- [corner] |> List.append myCorners //Check if the corner in the loop is occupied by our tile and if it is add it to myCorners list
        myCorners.Length //Return  the length of the myCorners list to get all the inhabited corners of the tile type we are checking


    // Evaluation function
    let Evaluation (validMoveFunc) (board : byte[,]) = //Evaluation function take a board

        let blackScore = GetScore board black //Get score for player black
        let whiteScore = GetScore board white //Get score for player white
        let blackMobility:(int * int)list = validMoveFunc board black //Get player blacks valid moves in the game //KALLA PÅ DENNA IFRÅN GAME:CS//
        let whiteMobility:(int * int)list = validMoveFunc board white //Get player blacks valid moves in the game //KALLA PÅ DENNA IFRÅN GAME:CS//

        if blackScore = 0 then -200000 //if black score = 0 then return -200000
        elif whiteScore = 0 then 200000 //if whites score = 0 then return 200000
        else //If none of bellow then nothing

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
   
    let MakeMove (getFlippedPFunc) (board : byte[,]) (move : (int * int)) (tile : byte) =
        let flippedPieces:(int*int)list = getFlippedPFunc board move tile //Runs the getflippedpieces function
        for flippedPiece in flippedPieces do //for every flipped piece
            board.[fst flippedPiece, snd flippedPiece] <- tile //change the existing tile on the board the the new tile
        if not(flippedPieces.IsEmpty) then //If any flipped piece was missed
            board.[fst move, snd move] <- tile //Change the tile
        board
        

    let GetWinner (validMoveFunc) (board : byte[,]) = 
       let whiteScore = GetScore board white //Get score for white
       let blackScore = GetScore board black //Get score for black
       if whiteScore = 0 || blackScore = 0 || whiteScore + blackScore = 64 || 
           List.length(validMoveFunc board black) + List.length(validMoveFunc board white) = 0 then // If statement to get stats for each player
           if blackScore > whiteScore then black //If blackScore greater then whiteScore then black return as winner
           elif whiteScore > blackScore then white //If whiteScore greater then blackScore then white return as winner
           else tie //If whiteScore is equal blackScore then return tie 
       else byte -1 // Return empty


    
   //MinimaxAlphaBeta recursive function
    let rec MinimaxAlphaBeta (getFlippedPFunc) (validMoveFunc) (board : byte[,]) (depth : int) (a : int) (b : int) (tile : byte) (isMaxPlayer : bool) =
        let rec RecMoveLoop (getFlippedPFunc) (board : byte[,]) (validMoves : (int*int)list) (bestScore : int) (tile : byte) (isMaxPlayer : bool) (a : int) (b : int) =
            match validMoves with //Matches validMoves
                | [] -> bestScore //If empty then return bestScore
                | head::tail -> //Check from start to end
                    let childBoard = MakeMove getFlippedPFunc board head tile //Add childBaord which calls for MakeMNove function
                    let nodeScore = MinimaxAlphaBeta getFlippedPFunc validMoveFunc childBoard (depth - 1) a b (OtherTile tile) (not isMaxPlayer) //nodeScore is MinimaxAlphaBeta
                    if isMaxPlayer then //Check if its max player
                        let newBestScore = max bestScore nodeScore //Max value of bestScore and nodeScore added to newBestScore
                        let newA = max bestScore a// max of bestScore and a added to newA
                        if b <= newA then //Check if newA is equal or greater then b
                            newBestScore // if so then return newBestScore
                        else //Else if b is greater or equal then newA
                            RecMoveLoop getFlippedPFunc board tail newBestScore tile isMaxPlayer newA b //Then return RecMoveLoop
                    else
                        let newBestScore = min bestScore nodeScore //Max value of bestScore and nodeScore is added to newBestScore
                        let newB = min bestScore b // max of bestScore and a is added to newB
                        if newB <= a then //Check if a is equal or greater then newB
                            newBestScore // if so then return newBestScore
                        else //Else if newB is greater or equal then a
                            RecMoveLoop getFlippedPFunc board tail newBestScore tile isMaxPlayer a newB //Then return RecMoveLoop


        if depth = 0 || GetWinner validMoveFunc board <> byte -1 then // Check if depth = 0 or if GetWinner is empty
            Evaluation validMoveFunc board //If so then return Evaluation board
        else //Else if depth != 0 or GetWinner != empty
            let bestScore = match isMaxPlayer with // Set bestScore and check with match if isMaxPlayer is true or false
                            | true -> System.Int32.MinValue //If true then MinValue
                            | false -> System.Int32.MaxValue //If false then MaxValue
            let validMoves:(int * int)list= validMoveFunc board tile // Get function validMoves from game.cs (C#)

            if validMoves.IsEmpty then //Check if validMoves = empty 
                MinimaxAlphaBeta getFlippedPFunc validMoveFunc board depth a b (OtherTile tile) (not isMaxPlayer) //If empty then MinimaxAlphaBeta
            else //Else if validMoves != empty 
                RecMoveLoop getFlippedPFunc board validMoves bestScore tile isMaxPlayer a b // If not empty then RecMoveLoop

                   


