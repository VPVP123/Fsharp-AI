﻿namespace FSAI

module Minimax =

    type Class1() = 
        member this.X = "F#"

    let boardSize = int 8
    let empty = byte  0
    let white = byte  1
    let black = byte  2
    let valid = byte  3
    let tie =  byte 4

    //Function check if it inside the board 8x8
    //let IsOnBoard x y = 
    //    0 <= x && x <= 7 && 0 <= y && y <= 7 //See if its inside board of 8x8 tiles

    let MinimaxAlphaBeta (board : byte[,]) (depth : int) (a : int) (b : int) (tile : byte) (isMaxPlayer : bool) =
        0 //Placeholder


    let otherTile tile = 
        if tile = black then white //check if player black then return player white
        elif tile = white then black //Opposit, check if player white then return player black
        else byte -1//error if not if npt value 1 or 2

    let GetScore (board : byte[,]) (tile : byte) =
        Seq.cast<byte> board  //Set start value
        |> Seq.filter (fun cell -> cell = tile)  |> Seq.length //Filter each cell in the board

    let GetValidMoves (board : byte[,]) (tile : byte) = 
        let randomList = [(1, 2); (2, 3); (4, 4)] //placeholder
        randomList //placeholder

    let CountCorners (board : byte[,]) (tile : byte) =
        let validCorners = [(board.[0,0]);(board.[0,7]);(board.[7,0]);(board.[7,7])] //Create a list of valid corners
        let mutable myCorners = [] //Create an empty list where we are going to append all the corners we are holding
        for corner in validCorners do // go over all corners in the first list we created
            if corner = tile then myCorners <- [corner] |> List.append myCorners //Check if the corner in the loop is occupied by our tile and if it is add it to myCorners list
        myCorners.Length //Return  the length of the myCorners list to get all the inhabited corners of the tile type we are checking

    let Evaluation (board : byte[,]) = //Evaluation function take a board

        let blackScore = GetScore board black //Get score for player black
        let whiteScore = GetScore board white //Get score for player white
        let blackMobility = GetValidMoves board black //Get player blacks valid moves in the game
        let whiteMobility = GetValidMoves board white //Get player blacks valid moves in the game

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
   


    let getWinner (board : byte[,]) = 
       let whiteScore = GetScore board white //Get score for white
       let blackScore = GetScore board black //Get score for black
       if whiteScore = 0 || blackScore = 0 || whiteScore + blackScore = 64 || 
           List.length(GetValidMoves board black) + List.length(GetValidMoves board white) = 0 then // If statement to get stats for each player
           if blackScore > whiteScore then black //If blackScore greater then whiteScore then black return as winner
           elif whiteScore > blackScore then white //If whiteScore greater then blackScore then white return as winner
           else tie //If whiteScore is equal blackScore then return tie 
       else byte -1// Error? or empty
   
   
       
