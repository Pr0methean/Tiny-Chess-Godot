namespace auto_Bot_1337;

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
    private static long MIN_REPEATED_POSITION_PENALTY = 200_000;
    private static long MAX_REPEATED_POSITION_PENALTY = 500_000;
    private static long SAME_PIECE_OPENING_MOVE_PENALTY = 1_000_000;
    private static long CHECK_BONUS = 600_000;
    private static long CASTLING_BONUS = 2_000_000;
    private static long[] PIECE_VALUES = { 0, 100, 305, 333, 563, 950, 1_000_000 };
    private static long[] PIECE_RANK_PUSH_VALUES = { 0, 300, 400, 600, 1200, 1600, 300 };
    private static long[] PIECE_FILE_PUSH_VALUES = { 0, 100, 750, 600, 1200, 1600, 300 };
    private static long[] PIECE_SWARM_VALUES = { 0, 50, 200, 333, 500, 500, 100 };
    private static long[] WHITE_PASSED_PAWN_VALUES = { 0, 0, 0, 0, 16, 64, 128, 0 };
    private static long[] BLACK_PASSED_PAWN_VALUES = { 0, 128, 64, 16, 0, 0, 0, 0 };
    private static int[] FILE_CENTER_DISTANCE_VALUES = { 6, 3, 1, 0, 0, 1, 3, 6 };
    private Random random = new();
    
    public Move Think(Board board, Timer timer)
    {
        // [Seb tweak start]- (adding tiny opening book for extra variety when playing against humans)
        if (board.PlyCount < 15)
        {
            Move bookMove = TinyOpeningBook.TryGetMove(board, randomlyDontUseBookProb: (board.PlyCount + 1.0) / 16);
            if (!bookMove.IsNull)
            {
                return bookMove;
            }
        }
        // [Seb tweak end]
        
        bool iAmWhite = board.IsWhiteToMove;
        int negateIfWhite = iAmWhite ? -1 : 1;
        Square enemyKing = board.GetKingSquare(!iAmWhite);
        int enemyKingRank = enemyKing.Rank;
        int enemyKingFile = enemyKing.File;
        ulong myBitboard = iAmWhite ? board.WhitePiecesBitboard : board.BlackPiecesBitboard;
        bool iAmABareKing = isBareKing(myBitboard);
        ulong opponentBitboard = iAmWhite ? board.BlackPiecesBitboard : board.WhitePiecesBitboard;
        bool opponentIsBareKing = isBareKing(opponentBitboard);
        long materialEval = 0;
        if (iAmABareKing) {
            Debug.WriteLine("Skipping material evaluation: I'm a bare king!");
            materialEval = long.MinValue;
        } else if (opponentIsBareKing) {
            Debug.WriteLine("Skipping material evaluation: Opponent is a bare king!");
            materialEval = long.MaxValue;
        } else {
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
        }
        long bestScore = long.MinValue;
        Move[] moves = board.GetLegalMoves();
        Move? bestMove = null;
        foreach (Move move in moves) {
            board.MakeMove(move);
            if (board.IsInCheckmate()) {
                board.UndoMove(move);
                return move;
            }
            long score;
            if (board.IsDraw()) {
                if (iAmABareKing)
                {
                    board.UndoMove(move);
                    return move;
                }
                if (materialEval < 0)
                {
                    // Opponent is ahead on material, so favor the draw
                    score = 0;
                }
                else
                {
                    score = long.MinValue;
                }
                goto scoreFinished;
            }
            Debug.WriteLine("Evaluating {0}", move);
            if (move.IsCapture) {
                ulong opponentBitboardAfterMove = iAmWhite ? board.BlackPiecesBitboard : board.WhitePiecesBitboard;
                if (isBareKing(opponentBitboardAfterMove))
                {
                    Debug.WriteLine("This move will leave the opponent a bare king!");
                    score = long.MaxValue;
                    goto scoreFinished;
                }
            }
            Move[] responses = board.GetLegalMoves();
            Debug.WriteLine("Responses: {0}", responses.Length);
            long? worstCapture = responses.Max<Move,long?>(response =>
            {
                (PieceType captured, PieceType counterCaptured) = bestExchange(board, response);
                return (PIECE_VALUES[(int)captured] * MY_PIECE_VALUE_MULTIPLIER
                                   - PIECE_VALUES[(int)counterCaptured] * ENEMY_PIECE_VALUE_MULTIPLIER);
            });
            Debug.WriteLine("Worst capturing response: {0}", worstCapture);
            score = -responses.Sum(m =>
                        PENALTY_PER_ENEMY_MOVE
                        + PIECE_VALUES[(int)m.CapturePieceType] * MY_PIECE_VALUE_PER_CAPTURING_MOVE_MULTIPLIER)
                    - Math.Max(0, worstCapture ?? 0L);
            Debug.WriteLine("Score based on responses: {0}", score);
            if (move.IsCapture)
            {
                long capture_bonus = PIECE_VALUES[(int)move.CapturePieceType];
                if (move.CapturePieceType == PieceType.Pawn)
                {
                    capture_bonus += iAmWhite
                        ? BLACK_PASSED_PAWN_VALUES[move.TargetSquare.Rank]
                        : WHITE_PASSED_PAWN_VALUES[move.TargetSquare.Rank];
                }
                capture_bonus *= ENEMY_PIECE_VALUE_MULTIPLIER;
                Debug.WriteLine("Capture bonus: {0}", capture_bonus);
                score += capture_bonus;
            }
            if (iAmABareKing)
            {
                goto scoreFinished;
            }

            if (board.PlyCount < 10 && move.MovePieceType != PieceType.Pawn 
                                    && move.StartSquare.Rank != 0 && move.StartSquare.Rank != 7)
            {
                Debug.WriteLine("Same piece opening move penalty: {0}", SAME_PIECE_OPENING_MOVE_PENALTY);
                score -= SAME_PIECE_OPENING_MOVE_PENALTY;
            }
            if (board.IsRepeatedPosition())
            {
                long penalty = random.NextInt64(MIN_REPEATED_POSITION_PENALTY, MAX_REPEATED_POSITION_PENALTY);
                Debug.WriteLine("Repeated position penalty: {0}", penalty);
                score -= penalty;
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
                goto scoreFinished;
            }
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
            int rankPushAdjustment = (move.StartSquare.Rank - move.TargetSquare.Rank) * negateIfWhite;
            int filePushAdjustment = FILE_CENTER_DISTANCE_VALUES[move.StartSquare.File] -
                                     FILE_CENTER_DISTANCE_VALUES[move.TargetSquare.File];
            Debug.WriteLine("Swarm: {0}, Rank Push: {1}, File Push: {2}", swarmAdjustment, rankPushAdjustment,
                            filePushAdjustment);
            score += PIECE_RANK_PUSH_VALUES[(int)move.MovePieceType] * rankPushAdjustment
                     + PIECE_FILE_PUSH_VALUES[(int)move.MovePieceType] * filePushAdjustment
                     + PIECE_SWARM_VALUES[(int)move.MovePieceType] * swarmAdjustment;
            scoreFinished:
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

    private static (PieceType, PieceType) bestExchange(Board board, Move response)
    {
        board.MakeMove(response);
        bool isMate = board.IsInCheckmate();
        (PieceType, PieceType) result;
        if (isMate)
        {
            result = (PieceType.King, PieceType.None);
        }
        else
        {
            PieceType captured = response.CapturePieceType;
            PieceType? counterCaptured = board.GetLegalMoves()
                .Max(m => (PieceType?) capturedOrMatedPieceType(board, m));
            result = (captured, counterCaptured ?? PieceType.None);
        }
        board.UndoMove(response);
        return result;
    }
    
    private static PieceType capturedOrMatedPieceType(Board board, Move response)
    {
        board.MakeMove(response);
        bool isMate = board.IsInCheckmate();
        if (!isMate && board.IsDraw())
        {
            ulong previousPlayerBitboard = board.IsWhiteToMove ? board.BlackPiecesBitboard : board.WhitePiecesBitboard;
            // Treat draw as a win for the bare king, since that's the best he can do
            isMate = isBareKing(previousPlayerBitboard);
        }
        board.UndoMove(response);
        if (isMate)
        {
            return PieceType.King;
        }

        return response.CapturePieceType;
    }

    private static bool isBareKing(ulong bitboard)
    {
        return (bitboard & (bitboard - 1)) == 0;
    }
}