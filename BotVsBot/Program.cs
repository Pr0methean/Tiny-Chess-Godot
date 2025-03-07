using System.Diagnostics;
using System.Numerics;
using auto_Bot_1337;
using ChessChallenge.Chess;
using Board = ChessChallenge.Chess.Board;
using Move = ChessChallenge.Chess.Move;
using Timer = ChessChallenge.API.Timer;

Bot_1337 white = new();
Bot_1337 black = new();
Board board = new();
board.LoadStartPosition();
bool whiteToMove = true;

ChessChallenge.API.Board apiBoard = new(board);
GameResult result = GameResult.InProgress;
do {
    Bot_1337 botToMove = whiteToMove ? white : black;
    ChessChallenge.API.Move move = botToMove.Think(apiBoard, new Timer(2000));
    Debug.WriteLine("Chosen move: " + move);
    apiBoard.MakeMove(move);
    whiteToMove = !whiteToMove;
    board.MakeMove(new Move(move.RawValue), false);
    if (Bot_1337.mateOrDrawCache[BitOperations.PopCount(apiBoard.AllPiecesBitboard) - 2].ContainsKey(board.ZobristKey)) {
        // Handles false positives due to Zobrist collisions
        result = Arbiter.GetGameState(board);
    } else if (Arbiter.isThreefoldRepetition(board)) {
        result = GameResult.Repetition;
    }
} while (result == GameResult.InProgress);
Console.WriteLine(PGNCreator.CreatePGN_InGameFormat(board, board.AllGameMoves.ToArray()));
Console.WriteLine(result);
