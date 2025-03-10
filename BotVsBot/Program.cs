using static ChessChallenge.API.PieceType;
using System.Diagnostics;
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
uint plyCount = 0;
do {
    Bot_1337 botToMove = whiteToMove ? white : black;
    ChessChallenge.API.Move move = botToMove.Think(apiBoard, new Timer(2000));
    Move cccMove = new Move(move.RawValue);
    if (plyCount % 2 == 0)
    {
        Console.Write(plyCount / 2 + 1 + ". ");
    }
    Console.Write(MoveUtility.GetMoveNameSAN(cccMove, board));
    if (plyCount % 2 != 0)
    {
        Console.WriteLine();
    } else {
        Console.Write("  ");
    }
    plyCount++;
    apiBoard.MakeMove(move);
    board.MakeMove(cccMove, false);
    if (Bot_1337.firstNonBookMove) {
        if (move.IsCapture || move.MovePieceType == Pawn 
                           || (move.MovePieceType is King or Rook && move.StartSquare.Rank == (whiteToMove ? 0 : 7))) {
            Bot_1337.trimCache(apiBoard);
        }

        var cacheEntry = Bot_1337.getAlphaBetaCacheEntry(Bot_1337.monotonicKey(apiBoard), apiBoard);
        if (cacheEntry is { QuietDepth: Byte.MaxValue, TotalDepth: Byte.MaxValue }) {
            // Handles false positives due to Zobrist collision
            result = Arbiter.GetGameState(board);
        }
        else if (Arbiter.isThreefoldRepetition(board)) {
            result = GameResult.Repetition;
        }
    }
    whiteToMove = !whiteToMove;
} while (result == GameResult.InProgress);
Console.WriteLine();
Console.WriteLine(result);
