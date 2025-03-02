﻿using System.Diagnostics;
using auto_Bot_1337;
using ChessChallenge.API;
using ChessChallenge.Chess;
using Board = ChessChallenge.Chess.Board;
using Move = ChessChallenge.Chess.Move;
using Timer = ChessChallenge.API.Timer;

Bot_1337 white = new();
Bot_1337 black = new();
Board board = new();
board.LoadStartPosition();
bool whiteToMove = true;
bool checkGameState(ChessChallenge.Chess.Board board)
{
    GameResult gameState = Arbiter.GetGameState(board);
    if (gameState != GameResult.InProgress)
    {
        Console.WriteLine(PGNCreator.CreatePGN_InGameFormat(board, board.AllGameMoves.ToArray()));
        Console.WriteLine(gameState);
        return false;
    }
    return true;
}

ChessChallenge.API.Board apiBoard = new(board);
do
{
    IChessBot botToMove = whiteToMove ? white : black;
    ChessChallenge.API.Move move = botToMove.Think(apiBoard, new Timer(2000));
    Debug.WriteLine("Chosen move: " + move);
    apiBoard.MakeMove(move);
    whiteToMove = !whiteToMove;
    board.MakeMove(new Move(move.RawValue), false);
} while (checkGameState(board));