namespace auto_Bot_1337;

using Microsoft.Extensions.Caching.Memory;
using System.Diagnostics;
using System.Linq;
using System;
using ChessChallenge.API;

public class Bot_1337 : IChessBot
{
    private static long MY_PIECE_VALUE_MULTIPLIER = 900_000;
    private static long ENEMY_PIECE_VALUE_MULTIPLIER = 1_000_000;
    private static long MY_PIECE_VALUE_PER_CAPTURING_MOVE_MULTIPLIER = 20_000;
    private static long PENALTY_PER_ENEMY_MOVE = 1_000_000;
    private static long MIN_REPEATED_POSITION_PENALTY = 800_000_000;
    private static long MAX_REPEATED_POSITION_PENALTY = 2_800_000_000;
    private static long SAME_PIECE_OPENING_MOVE_PENALTY = 1_000_000;
    private static long CHECK_BONUS = 600_000;
    private static long CASTLING_BONUS = 2_000_000;
    private static long[] PIECE_VALUES = { 0, 100, 305, 333, 563, 950, 20_000 };
    private static long[] PIECE_RANK_PUSH_VALUES = { 0, 200, 300, 400, 800, 1200, 300 };
    private static long[] PIECE_FILE_PUSH_VALUES = { 0, 100, 500, 400, 600, 1200, 300 };
    private static long[] PIECE_SWARM_VALUES = { 0, 75, 150, 300, 400, 450, 200 };
    private static long[] WHITE_PASSED_PAWN_VALUES = { 0, 0, 0, 0, 16, 64, 128, 0 };
    private static long[] BLACK_PASSED_PAWN_VALUES = { 0, 128, 64, 16, 0, 0, 0, 0 };
    private static long[] FILE_CENTER_DISTANCE_VALUES = { 6, 3, 1, 0, 0, 1, 3, 6 };
    private static long[] WHITE_RANK_ADVANCEMENT_VALUES = { 0, 3, 5, 7, 9, 11, 12, 13 };
    private static long[] BLACK_RANK_ADVANCEMENT_VALUES = { 13, 12, 11, 9, 7, 5, 3, 0 };
    private Random random = new();
    private MemoryCache materialEvalZobrist = new(new MemoryCacheOptions());
    private MemoryCache moveScoreZobrist = new(new MemoryCacheOptions());
    private MemoryCache bestResponseToResponseZobrist = new(new MemoryCacheOptions());
    public Move Think(Board board, Timer timer)
    {
        // [Seb tweak start]- (adding tiny opening book for extra variety when playing against humans)
        if (board.PlyCount < 32)
        {
            Move bookMove = TinyOpeningBook.TryGetMove(board, randomlyDontUseBookProb: (board.PlyCount) / 32);
            if (!bookMove.IsNull)
            {
                return bookMove;
            }
        }
        // [Seb tweak end]
        bool iAmWhite = board.IsWhiteToMove;
        int negateIfWhite = iAmWhite ? -1 : 1;
        ulong myBitboard = iAmWhite ? board.WhitePiecesBitboard : board.BlackPiecesBitboard;
        bool iAmABareKing = isBareKing(myBitboard);
        long materialEval = materialEvalZobrist.GetOrCreate(board.ZobristKey, _ =>
        {
            if (iAmABareKing) {
                Debug.WriteLine("Skipping material evaluation: I'm a bare king!");
                return -100_000_000_000L;
            } else {
                ulong opponentBitboard = iAmWhite ? board.BlackPiecesBitboard : board.WhitePiecesBitboard;
                bool opponentIsBareKing = isBareKing(opponentBitboard);
                if (opponentIsBareKing)
                {
                    Debug.WriteLine("Skipping material evaluation: Opponent is a bare king!");
                    return 100_000_000_000L;
                }
            }
            long materialEval = 0;
            PieceList[] plist = board.GetAllPieceLists();
            foreach (PieceList list in plist) {
                bool isWhite = list.IsWhitePieceList;
                if (isWhite) {
                    materialEval -= (list.Count * PIECE_VALUES[(int)list.TypeOfPieceInList]);
                    if (list.TypeOfPieceInList == PieceType.Pawn)
                    {
                        foreach (var piece in list)
                        {
                            materialEval -= WHITE_PASSED_PAWN_VALUES[piece.Square.Rank];
                        }
                    }
                } else {
                    materialEval += (list.Count * PIECE_VALUES[(int)list.TypeOfPieceInList]);
                    if (list.TypeOfPieceInList == PieceType.Pawn)
                    {
                        foreach (var piece in list)
                        {
                            materialEval += BLACK_PASSED_PAWN_VALUES[piece.Square.Rank];
                        }
                    }
                }
            }
            materialEval *= negateIfWhite;
            Debug.WriteLine("Material advantage: {0}", materialEval);
            return materialEval;
        });
        long fiftyMoveResetValue = 0;
        long fiftyMoveCounter = board.FiftyMoveCounter;
        if (materialEval > 0 && fiftyMoveCounter >= 60)
        {
            // Player that's ahead wants to reset the 50-move-rule counter.
            fiftyMoveResetValue = fiftyMoveCounter * fiftyMoveCounter * fiftyMoveCounter * fiftyMoveCounter * fiftyMoveCounter;
            Debug.WriteLine("Fifty-move reset value: {0}", fiftyMoveResetValue);
        }
        long bestScore = long.MinValue;
        board.MakeMove(Move.NullMove);
        long baseline = moveScoreZobrist.GetOrCreate(HashCode.Combine(board.ZobristKey, Move.NullMove),
            _ => evaluateMadeMove(board, iAmABareKing, materialEval, Move.NullMove, iAmWhite, negateIfWhite, 0));
        
        // Sanity checks
        baseline = Math.Max(Math.Min(baseline, 1_000_000_000), -1_000_000_000);

        board.UndoMove(Move.NullMove);
        Debug.WriteLine("Null move has baseline score of {0}", baseline);
        Move[] moves = board.GetLegalMoves();
        Move? bestMove = null; 
        foreach (Move move in moves) {
            board.MakeMove(move);
            long score = moveScoreZobrist.GetOrCreate(HashCode.Combine(board.ZobristKey, move), 
                _ => evaluateMadeMove(board, iAmABareKing, materialEval, move, iAmWhite, negateIfWhite, baseline));
            // Repeated positions don't factor into the Zobrist hash, so the penalty for them must be applied separately
            if (board.IsRepeatedPosition())
            {
                long penalty = random.NextInt64(MIN_REPEATED_POSITION_PENALTY, MAX_REPEATED_POSITION_PENALTY);
                Debug.WriteLine("Repeated position penalty: {0}", penalty);
                score -= penalty;
            }
            if (fiftyMoveResetValue != 0 && (move.IsCapture || move.MovePieceType == PieceType.Pawn))
            {
                score += fiftyMoveResetValue;
            }
            board.UndoMove(move);
            Debug.WriteLine("Move {0} has score {1}", move, score);
            if (bestMove == null || score > bestScore || (score == bestScore && random.Next(2) == 0))
            {
                bestScore = score;
                bestMove = move;
            }
        }
        return (Move)bestMove!;
    }

    private long evaluateMadeMove(Board board, bool iAmABareKing, long materialEval, Move move, bool iAmWhite,
        int negateIfWhite, long baseline)
    {
        var mateOrDraw = evaluateMateOrDraw(board, iAmABareKing, materialEval, baseline);
        if (mateOrDraw != null)
        {
            return (long) mateOrDraw;
        }
        Debug.WriteLine("Evaluating {0}", move);
        Move[] responses = board.GetLegalMoves();
        Debug.WriteLine("Responses: {0}", responses.Length);
        long bestResponseScore = long.MinValue;
        foreach (var response in responses)
        {
            PieceType capturedInResponse = response.CapturePieceType;
            board.MakeMove(response);
            long? responseScore = bestResponseToResponseZobrist.GetOrCreate(board.ZobristKey, _ =>
            {
                var mateOrDrawInResponse = evaluateMateOrDraw(board, iAmABareKing, materialEval, baseline); 
                if (mateOrDrawInResponse != null)
                {
                    return -mateOrDrawInResponse;
                }
                return evalCaptureBonus(board, response, !iAmWhite, MY_PIECE_VALUE_MULTIPLIER)
                    - board.GetLegalMoves().Max(responseToResponse =>
                {
                    board.MakeMove(responseToResponse);
                    long? responseToResponseScore = (long?)moveScoreZobrist.Get(HashCode.Combine(board.ZobristKey, responseToResponse));
                    responseToResponseScore ??= evaluateMateOrDraw(board, iAmABareKing, materialEval, baseline);
                    responseToResponseScore ??= evalCaptureBonus(board, move, iAmWhite, ENEMY_PIECE_VALUE_MULTIPLIER);
                    board.UndoMove(responseToResponse);
                    return responseToResponseScore;
                }) ?? 0L;
            });
            board.UndoMove(response);
            Debug.WriteLine("Response {0} has score {1}", response, responseScore);
            if (responseScore != null && responseScore > bestResponseScore)
            {
                bestResponseScore = (long) responseScore;
            }
        }
        var score = -responses.Sum(m =>
                        PENALTY_PER_ENEMY_MOVE
                        + PIECE_VALUES[(int)m.CapturePieceType] * MY_PIECE_VALUE_PER_CAPTURING_MOVE_MULTIPLIER)
                    - bestResponseScore;
        Debug.WriteLine("Score based on responses: {0}", score);
        if (move.IsCapture)
        {
            var capture_bonus = evalCaptureBonus(board, move, iAmWhite, ENEMY_PIECE_VALUE_MULTIPLIER);
            Debug.WriteLine("Capture bonus: {0}", capture_bonus);
            score += capture_bonus;
        }

        if (iAmABareKing)
        {
            goto scoreFinishedExceptBaselining;
        }

        if (board.PlyCount < 10 && (move.MovePieceType is PieceType.Bishop or PieceType.Rook or PieceType.Queen
                                    || move is { MovePieceType: PieceType.Knight, TargetSquare.Rank: 0 or 7 })
                                && move.StartSquare.Rank != 0 && move.StartSquare.Rank != 7 
                                && move.StartSquare.Rank != move.TargetSquare.Rank)
        {
            Debug.WriteLine("Same piece opening move penalty: {0}", SAME_PIECE_OPENING_MOVE_PENALTY);
            score -= SAME_PIECE_OPENING_MOVE_PENALTY;
        }

        if (board.IsInCheck())
        {
            Debug.WriteLine("Check bonus: {0}", CHECK_BONUS);
            score += CHECK_BONUS;
        }

        if (move.IsPromotion)
        {
            score += MY_PIECE_VALUE_MULTIPLIER * (PIECE_VALUES[(int)move.PromotionPieceType] - 100);
        }

        if (move.IsCastles)
        {
            Debug.WriteLine("Castling bonus: {0}", CASTLING_BONUS);
            score += CASTLING_BONUS;
            // Push/swarm logic won't handle castling well, so skip it
            goto scoreFinishedExceptBaselining;
        }

        if (move.IsNull)
        {
            goto scoreFinishedExceptBaselining;
        }
        Square enemyKing = board.GetKingSquare(!iAmWhite);
        int enemyKingRank = enemyKing.Rank;
        int enemyKingFile = enemyKing.File;
        // Push/swarm adjustment
        int ranksBehindKingBefore = (move.StartSquare.Rank - enemyKingRank) * negateIfWhite;
        int ranksBehindKingAfter = (move.TargetSquare.Rank - enemyKingRank) * negateIfWhite;
        if (move.MovePieceType == PieceType.Pawn)
        {
            ranksBehindKingBefore = Math.Max(ranksBehindKingBefore, 0);
            ranksBehindKingAfter = Math.Max(ranksBehindKingAfter, 0);
        }

        int squaresFromKingBefore = Math.Min(Math.Abs(ranksBehindKingBefore),
            Math.Abs(move.StartSquare.File - enemyKingFile));
        int squaresFromKingAfter = Math.Min(Math.Abs(ranksBehindKingAfter),
            Math.Abs(move.TargetSquare.File - enemyKingFile));
        int swarmAdjustment = squaresFromKingBefore - squaresFromKingAfter;
        long[] myRankValues = iAmWhite ? WHITE_RANK_ADVANCEMENT_VALUES : BLACK_RANK_ADVANCEMENT_VALUES;
        long rankPushAdjustment = myRankValues[move.StartSquare.Rank] - myRankValues[move.TargetSquare.Rank];
        long filePushAdjustment;
        bool behindUnmovedPawn;
        if (iAmWhite)
        {
            if (move.TargetSquare.Rank != 7)
            {
                behindUnmovedPawn = false;
            }
            Piece inFront = board.GetPiece(new Square(move.TargetSquare.File, 6));
            behindUnmovedPawn = inFront.IsPawn && inFront.IsWhite;
        }
        else
        {
            if (move.TargetSquare.Rank != 0)
            {
                behindUnmovedPawn = false;
            }
            Piece inFront = board.GetPiece(new Square(move.TargetSquare.File, 1));
            behindUnmovedPawn = inFront.IsPawn && !inFront.IsWhite;
        }
        if (behindUnmovedPawn) {
            filePushAdjustment = 0; // No push adjustment applies on the back rank 
        }
        else
        {
            filePushAdjustment = FILE_CENTER_DISTANCE_VALUES[move.StartSquare.File] -
                                 FILE_CENTER_DISTANCE_VALUES[move.TargetSquare.File];
        }

        Debug.WriteLine("Swarm: {0}, Rank Push: {1}, File Push: {2}", swarmAdjustment, rankPushAdjustment,
            filePushAdjustment);
        score += PIECE_RANK_PUSH_VALUES[(int)move.MovePieceType] * rankPushAdjustment
                 + PIECE_FILE_PUSH_VALUES[(int)move.MovePieceType] * filePushAdjustment
                 + PIECE_SWARM_VALUES[(int)move.MovePieceType] * swarmAdjustment;
        scoreFinishedExceptBaselining:
        Debug.WriteLine("Score before baselining: {0}", score);
        score -= baseline;
        return score;
    }

    private static long evalCaptureBonus(Board board, Move move, bool iAmWhite, long pieceValueMultiplier)
    {
        long capture_bonus;
        ulong opponentBitboardAfterMove = iAmWhite ? board.BlackPiecesBitboard : board.WhitePiecesBitboard;
        if (isBareKing(opponentBitboardAfterMove))
        {
            Debug.WriteLine("This move will leave the opponent a bare king!");
            capture_bonus = 100_000_000_000L;
        } else {
            capture_bonus = PIECE_VALUES[(int)move.CapturePieceType];
            if (move.CapturePieceType == PieceType.Pawn)
            {
                capture_bonus += iAmWhite
                    ? BLACK_PASSED_PAWN_VALUES[move.TargetSquare.Rank]
                    : WHITE_PASSED_PAWN_VALUES[move.TargetSquare.Rank];
            }
            capture_bonus *= pieceValueMultiplier;
        }

        return capture_bonus;
    }

    private static long? evaluateMateOrDraw(Board board, bool iAmABareKing, long materialEval, long baseline)
    {
        if (board.IsInCheckmate())
        {
            return 1_000_000_000_000L;
        }

        if (board.IsDraw())
        {
            if (iAmABareKing)
            {
                // A draw is as good as a win for a bare king since it's the best he can do
                return 1_000_000_000_000L;
            }
            if (materialEval < 0)
            {
                // Opponent is ahead on material, so favor the draw
                return Math.Max(baseline, 0L);
            }
            return -500_000_000_000L;
        }

        return null;
    }

    private static bool isBareKing(ulong bitboard)
    {
        return (bitboard & (bitboard - 1)) == 0;
    }
}