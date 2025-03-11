using static ChessChallenge.API.PieceType;
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
    string moveNameSan = MoveUtility.GetMoveNameSAN(cccMove, board);
    if (plyCount % 2 == 0)
    {
        Console.Write("{0}. {1}  ", plyCount / 2 + 1, moveNameSan);
    } else {
        Console.WriteLine(moveNameSan);
    }
    if (moveNameSan.EndsWith('#')) {
        result = whiteToMove ? GameResult.BlackIsMated : GameResult.WhiteIsMated;
        break;
    }
    plyCount++;
    apiBoard.MakeMove(move);
    board.MakeMove(cccMove, false);
    if (Bot_1337.firstNonBookMove) {
        result = Arbiter.GetGameState(board);
        if (result == GameResult.InProgress && (move.IsCapture || move.MovePieceType == Pawn 
                             || (move.MovePieceType is King or Rook && move.StartSquare.Rank == (whiteToMove ? 0 : 7)))) {
            Bot_1337.trimCache(apiBoard);
        }
    }
    whiteToMove = !whiteToMove;
} while (result == GameResult.InProgress);
Console.WriteLine("\n{0}", result);
