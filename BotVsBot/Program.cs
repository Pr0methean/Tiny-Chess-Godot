using System.Diagnostics;
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

ChessChallenge.API.Board apiBoard = new(board);
GameResult result = GameResult.InProgress;
int monotonicKey = Bot_1337.MAX_MONOTONIC_KEY;
int oldMonotonicKey = Bot_1337.MAX_MONOTONIC_KEY;
do {
    Bot_1337 botToMove = whiteToMove ? white : black;
    ChessChallenge.API.Move move = botToMove.Think(apiBoard, new Timer(2000));
    Debug.WriteLine("Chosen move: " + move);
    apiBoard.MakeMove(move);
    whiteToMove = !whiteToMove;
    board.MakeMove(new Move(move.RawValue), false);
    if (move.IsCapture || move.MovePieceType == PieceType.Pawn) {
        monotonicKey = Bot_1337.monotonicKey(apiBoard);
        if (monotonicKey != oldMonotonicKey) {
            Bot_1337.trimCache(monotonicKey);
            oldMonotonicKey = monotonicKey;
        }
    }
    if (Bot_1337.mateOrDrawCache[monotonicKey].ContainsKey(board.ZobristKey)) {
        // Handles false positives due to Zobrist collision
        result = Arbiter.GetGameState(board);
    } else if (Arbiter.isThreefoldRepetition(board)) {
        result = GameResult.Repetition;
    }
} while (result == GameResult.InProgress);
Console.WriteLine(PGNCreator.CreatePGN_InGameFormat(board, board.AllGameMoves.ToArray()));
Console.WriteLine(result);
