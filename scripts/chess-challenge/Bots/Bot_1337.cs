namespace auto_Bot_1337;

using System.Diagnostics;
using System.Linq;
using System;
using ChessChallenge.API;

public class Bot_1337 : IChessBot {
    private static long MY_PIECE_VALUE_MULTIPLIER = 900_000;
    private static long ENEMY_PIECE_VALUE_MULTIPLIER = 1_000_000;
    private static long MY_PIECE_VALUE_PER_CAPTURING_MOVE_MULTIPLIER = 20_000;
    private static long PENALTY_PER_ENEMY_MOVE = 1_000_000;
    private static long MIN_DRAW_VALUE_WHEN_BEHIND = 0;
    private static long MAX_DRAW_VALUE_WHEN_BEHIND = 500_000_000;
    private static long MAX_MOVE_VALUE_NOISE = 1_000;
    private static long SAME_PIECE_OPENING_MOVE_PENALTY = 1_000_000;
    private static long ENEMY_CHECK_BONUS = 1_000_000_000;
    private static long CHECK_BONUS = 1_000_000;
    private static long CASTLING_BONUS = 2_000_000;
    private static long[] PIECE_VALUES = { 0, 100, 305, 333, 563, 950, 20_000 };
    private static long[] PIECE_RANK_PUSH_VALUES = { 0, 400, 400, 800, 1200, 1600, 400 };
    private static long[] PIECE_FILE_PUSH_VALUES = { 0, 100, 500, 400, 600, 1200, 300 };
    private static long[] PIECE_SWARM_VALUES = { 0, 75, 150, 300, 400, 450, 200 };
    private static long[] WHITE_PASSED_PAWN_VALUES = { 0, 0, 0, 0, 16, 64, 128, 0 };
    private static long[] BLACK_PASSED_PAWN_VALUES = { 0, 128, 64, 16, 0, 0, 0, 0 };
    private static long[] FILE_CENTER_DISTANCE_VALUES = { 6, 3, 1, 0, 0, 1, 3, 6 };
    private static long[] WHITE_RANK_ADVANCEMENT_VALUES = { 0, 3, 6, 9, 11, 13, 14, 15 };
    private static long[] BLACK_RANK_ADVANCEMENT_VALUES = { 15, 14, 13, 11, 9, 6, 3, 0 };
    private static Random random = new();
    
    private static Dictionary<ulong, long> materialEvalZobrist = new();
    private static Dictionary<ulong, Move[]> legalMovesZobrist = new();
    public Dictionary<ulong, long?> mateOrDrawZobrist = new();
    private Dictionary<UInt128, long> moveScoreZobrist = new();
    private Dictionary<ulong, long> responseScoreZobrist = new();

    public Move Think(Board board, Timer timer) {
        // [Seb tweak start]- (adding tiny opening book for extra variety when playing against humans)
        if (board.PlyCount < 32) {
            Move bookMove = TinyOpeningBook.TryGetMove(board, randomlyDontUseBookProb: board.PlyCount / 32.0);
            if (!bookMove.IsNull) {
                return bookMove;
            }
        }

        // [Seb tweak end]
        bool iAmWhite = board.IsWhiteToMove;
        int negateIfWhite = iAmWhite ? -1 : 1;
        ulong myBitboard = iAmWhite ? board.WhitePiecesBitboard : board.BlackPiecesBitboard;
        bool iAmABareKing = isBareKing(myBitboard);
        ulong opponentBitboard = iAmWhite ? board.BlackPiecesBitboard : board.WhitePiecesBitboard;
        bool opponentIsBareKing = isBareKing(opponentBitboard);
        long materialEval = evaluateMaterial(board, iAmWhite);
        long fiftyMoveResetValue = 0;
        long fiftyMoveCounter = board.FiftyMoveCounter;
        if (materialEval > 0 && fiftyMoveCounter >= 60) {
            // Player that's ahead wants to reset the 50-move-rule counter.
            fiftyMoveResetValue = fiftyMoveCounter * fiftyMoveCounter * fiftyMoveCounter * 10_000;
            Debug.WriteLine("Fifty-move reset value: {0}", fiftyMoveResetValue);
        }

        board.MakeMove(Move.NullMove);
        // Prevents a bias against previously depth-1-evaluated moves when considering them as responses to responses
        long minOpptMovesBaseline = minOpptMovesScore(getLegalMoves(board));
        board.UndoMove(Move.NullMove);
        long bestScore = long.MinValue;
        /*
        board.MakeMove(Move.NullMove);
        long baseline = moveScoreZobrist.GetOrCreate(HashCode.Combine(board.ZobristKey, Move.NullMove),
            () => evaluateMadeMove(board, iAmABareKing, materialEval, Move.NullMove, iAmWhite, negateIfWhite, 0));

        // Sanity checks
        baseline = Math.Max(Math.Min(baseline, 1_000_000_000), -1_000_000_000);

        board.UndoMove(Move.NullMove);
        Debug.WriteLine("Null move has baseline score of {0}", baseline);
        */
        Move? bestMove = null;
        Move[] moves = getLegalMoves(board);
        foreach (Move move in moves) {
            board.MakeMove(move);
            // Repeated positions don't factor into the Zobrist hash, but the API treats them as draws (even if they've
            // only occurred once before)
            long score;
            if (board.IsRepeatedPosition()) {
                score = evaluateDraw(iAmABareKing, materialEval);
            } else {
                score = moveScoreZobrist.GetOrCreate(board.ZobristKey | (UInt128)move.RawValue << 64,
                    () => {
                        var mateOrDraw = evaluateMateOrDraw(board, iAmABareKing, materialEval);
                        if (mateOrDraw != null) {
                            return (long)mateOrDraw;
                        }

                        Debug.WriteLine("Evaluating {0}", move);
                        long score = responseScoreZobrist.GetOrCreate(board.ZobristKey, () => {
                            Move[] responses = getLegalMoves(board);
                            Debug.WriteLine("Responses: {0}", responses.Length);
                            long bestResponseScore = long.MinValue;
                            foreach (var response in responses) {
                                board.MakeMove(response);
                                long responseScore;
                                var mateOrDrawInResponse = evaluateMateOrDraw(board, opponentIsBareKing, -materialEval);
                                if (mateOrDrawInResponse != null) {
                                    responseScore = (long) (mateOrDrawInResponse);
                                } else {
                                    long responseCaptureBonus = evalCaptureBonus(board, response, !iAmWhite, MY_PIECE_VALUE_MULTIPLIER);
                                    long responseCheckBonus = board.IsInCheck() ? ENEMY_CHECK_BONUS : 0;
                                    long responsePromotionBonus = response.IsPromotion
                                        ? PIECE_VALUES[(int)response.PromotionPieceType] * ENEMY_PIECE_VALUE_MULTIPLIER
                                        : 0;
                                    long bestResponseToResponseScore = long.MinValue;
                                    foreach (var responseToResponse in getLegalMoves(board)) {
                                        board.MakeMove(responseToResponse);
                                        long responseToResponseScore;
                                        if (!moveScoreZobrist.TryGetValue(board.ZobristKey | (UInt128)responseToResponse.RawValue << 64,
                                                out responseToResponseScore)) {
                                            responseToResponseScore = evaluateMateOrDraw(board, iAmABareKing, materialEval)
                                                                      ??
                                                                      evalCaptureBonus(board, responseToResponse, iAmWhite,
                                                                          ENEMY_PIECE_VALUE_MULTIPLIER);
                                        }
                                        board.UndoMove(responseToResponse);
                                        if (responseToResponseScore >= 1_000_000_000_000) {
                                            bestResponseToResponseScore = responseToResponseScore;
                                            break;
                                        }
                                        if (responseToResponseScore > bestResponseToResponseScore) {
                                            bestResponseToResponseScore = responseToResponseScore;
                                        }
                                    }

                                    responseScore = responseCaptureBonus + responseCheckBonus + responsePromotionBonus -
                                                    bestResponseToResponseScore;
                                }
                                Debug.WriteLine("Response {0} has score {1}", response, responseScore);
                                board.UndoMove(response);
                                if (responseScore >= 1_000_000_000_000) {
                                    bestResponseScore = responseScore;
                                    break;
                                }
                                if (responseScore > bestResponseScore) {
                                    bestResponseScore = responseScore;
                                }
                            }
                            return minOpptMovesScore(responses) - bestResponseScore;
                        }) - minOpptMovesBaseline;
                        Debug.WriteLine("Score based on responses: {0}", score);
                        if (move.IsCapture) {
                            var capture_bonus = evalCaptureBonus(board, move, iAmWhite, ENEMY_PIECE_VALUE_MULTIPLIER);
                            Debug.WriteLine("Capture bonus: {0}", capture_bonus);
                            score += capture_bonus;
                        }

                        if (iAmABareKing) {
                            goto scoreFinishedExceptNoise;
                        }

                        if (board.IsInCheck()) {
                            Debug.WriteLine("Check bonus: {0}", CHECK_BONUS);
                            score += CHECK_BONUS;
                        }

                        if (move.IsCastles) {
                            Debug.WriteLine("Castling bonus: {0}", CASTLING_BONUS);
                            score += CASTLING_BONUS;
                            // Push/swarm logic won't handle castling well, so skip it
                            goto scoreFinishedExceptNoise;
                        }

                        if (board.PlyCount < 10 && (move.MovePieceType is PieceType.Bishop or PieceType.Rook or PieceType.Queen
                                                    || move is { MovePieceType: PieceType.Knight, TargetSquare.Rank: 0 or 7 })
                                                && move.StartSquare.Rank != 0 && move.StartSquare.Rank != 7) {
                            Debug.WriteLine("Same piece opening move penalty: {0}", SAME_PIECE_OPENING_MOVE_PENALTY);
                            score -= SAME_PIECE_OPENING_MOVE_PENALTY;
                        }

                        PieceType pushingPiece = move.MovePieceType;
                        if (move.IsPromotion) {
                            score += MY_PIECE_VALUE_MULTIPLIER * PIECE_VALUES[(int)move.PromotionPieceType];
                            pushingPiece = move.PromotionPieceType;
                        }

                        Square enemyKing = board.GetKingSquare(!iAmWhite);
                        int enemyKingRank = enemyKing.Rank;
                        int enemyKingFile = enemyKing.File;
                        // Push/swarm adjustment
                        int ranksBehindKingBefore = (move.StartSquare.Rank - enemyKingRank) * negateIfWhite;
                        int ranksBehindKingAfter = (move.TargetSquare.Rank - enemyKingRank) * negateIfWhite;
                        if (move.MovePieceType == PieceType.Pawn) {
                            ranksBehindKingBefore = Math.Max(ranksBehindKingBefore, 0);
                            ranksBehindKingAfter = Math.Max(ranksBehindKingAfter, 0);
                        }

                        int squaresFromKingBefore = Math.Min(Math.Abs(ranksBehindKingBefore),
                            Math.Abs(move.StartSquare.File - enemyKingFile));
                        int squaresFromKingAfter = Math.Min(Math.Abs(ranksBehindKingAfter),
                            Math.Abs(move.TargetSquare.File - enemyKingFile));
                        int swarmAdjustment = squaresFromKingBefore - squaresFromKingAfter;
                        long[] myRankValues = iAmWhite ? WHITE_RANK_ADVANCEMENT_VALUES : BLACK_RANK_ADVANCEMENT_VALUES;
                        long rankPushAdjustment = myRankValues[move.TargetSquare.Rank] - myRankValues[move.StartSquare.Rank];
                        long filePushAdjustment = FILE_CENTER_DISTANCE_VALUES[move.StartSquare.File] -
                                                 FILE_CENTER_DISTANCE_VALUES[move.TargetSquare.File];
                        if (filePushAdjustment > 0) {
                            bool targetBehindUnmovedPawn;
                            if (iAmWhite) {
                                if (move.TargetSquare.Rank != 0) {
                                    targetBehindUnmovedPawn = false;
                                }
                                else {
                                    Piece inFront = board.GetPiece(new Square(move.TargetSquare.File, 1));
                                    targetBehindUnmovedPawn = inFront.IsPawn && inFront.IsWhite;
                                }
                            } else {
                                if (move.TargetSquare.Rank != 7) {
                                    targetBehindUnmovedPawn = false;
                                }
                                else {
                                    Piece inFront = board.GetPiece(new Square(move.TargetSquare.File, 6));
                                    targetBehindUnmovedPawn = inFront.IsPawn && !inFront.IsWhite;
                                }
                            }
                            if (targetBehindUnmovedPawn) {
                                filePushAdjustment = 0; // No push adjustment applies on the back rank 
                            }
                        }

                        Debug.WriteLine("Swarm: {0}, Rank Push: {1}, File Push: {2}", swarmAdjustment, rankPushAdjustment,
                            filePushAdjustment);
                        score += PIECE_RANK_PUSH_VALUES[(int)pushingPiece] * rankPushAdjustment
                                  + PIECE_FILE_PUSH_VALUES[(int)pushingPiece] * filePushAdjustment
                                  + PIECE_SWARM_VALUES[(int)pushingPiece] * swarmAdjustment;
                        scoreFinishedExceptNoise:
                        Debug.WriteLine("Score before baselining and noise: {0}", score);
                        // score -= baseline;
                        return score;
                    });
                if (move.IsCapture || move.MovePieceType == PieceType.Pawn) {
                    score += fiftyMoveResetValue;
                }
                score += random.NextInt64(MAX_MOVE_VALUE_NOISE) - random.NextInt64(MAX_MOVE_VALUE_NOISE);
            }

            board.UndoMove(move);
            Debug.WriteLine("Move {0} has score {1}", move, score);
            if (score >= 1_000_000_000_000) {
                return move;
            }
            if (bestMove == null || score > bestScore || (score == bestScore && random.Next(2) == 0)) {
                bestScore = score;
                bestMove = move;
            }
        }

        return (Move)bestMove!;
    }

    private static Move[] getLegalMoves(Board board) {
        return legalMovesZobrist.GetOrCreate(board.ZobristKey, () => board.GetLegalMoves())!;
    }

    private static long evaluateMaterial(Board board, bool iAmWhite) {
        return materialEvalZobrist.GetOrCreate(board.ZobristKey, () => {
            if (isBareKing(board.WhitePiecesBitboard)) {
                Debug.WriteLine("Skipping material evaluation: white is a bare king!");
                return 100_000_000_000L;
            }

            if (isBareKing(board.BlackPiecesBitboard)) {
                Debug.WriteLine("Skipping material evaluation: black is a bare king!");
                return -100_000_000_000L;
            }

            long materialEval = 0;
            for (int i = 0; i < 64; i++) {
                Square square = new Square(i);
                Piece piece = board.GetPiece(square);
                bool isWhite = piece.IsWhite;
                materialEval += PIECE_VALUES[(int)piece.PieceType] * (isWhite ? -1 : 1);
                if (piece.IsPawn) {
                    materialEval += (isWhite ? WHITE_PASSED_PAWN_VALUES : BLACK_PASSED_PAWN_VALUES)
                        [square.Rank] * (isWhite ? -1 : 1);
                }
            }

            Debug.WriteLine("Material eval: {0}", materialEval);
            return materialEval;
        }) * (iAmWhite ? -1 : 1);
    }

    private static long minOpptMovesScore(Move[] responses) {
        return -responses.Sum(m =>
            PENALTY_PER_ENEMY_MOVE
            + PIECE_VALUES[(int)m.CapturePieceType] * MY_PIECE_VALUE_PER_CAPTURING_MOVE_MULTIPLIER);
    }

    private static long evalCaptureBonus(Board board, Move move, bool iAmWhite, long pieceValueMultiplier) {
        if (!move.IsCapture) {
            return 0;
        }
        ulong opponentBitboardAfterMove = iAmWhite ? board.BlackPiecesBitboard : board.WhitePiecesBitboard;
        if (isBareKing(opponentBitboardAfterMove)) {
            Debug.WriteLine("This move will leave the opponent a bare king!");
            return 600_000_000_000L;
        }
        else {
            long capture_bonus = PIECE_VALUES[(int)move.CapturePieceType];
            if (move.CapturePieceType == PieceType.Pawn) {
                capture_bonus += iAmWhite
                    ? BLACK_PASSED_PAWN_VALUES[move.TargetSquare.Rank]
                    : WHITE_PASSED_PAWN_VALUES[move.TargetSquare.Rank];
            }

            return capture_bonus * pieceValueMultiplier;
        }
    }

    private long? evaluateMateOrDraw(Board board, bool iAmABareKing, long materialEval) {
        return mateOrDrawZobrist.GetOrCreate(board.ZobristKey, () => {
            if (getLegalMoves(board).Length == 0) {
                if (board.IsInCheck()) {
                    // Checkmate
                    return 1_000_000_000_000L;
                }

                // Stalemate
                return evaluateDraw(iAmABareKing, materialEval);
            }

            if (board.IsFiftyMoveDraw() || board.IsInsufficientMaterial() || board.IsRepeatedPosition()) {
                return evaluateDraw(iAmABareKing, materialEval);
            }

            return null;
        });
    }

    private static long evaluateDraw(bool iAmABareKing, long materialEval) {
        if (iAmABareKing) {
            // A draw is almost as good as a win for a bare king since it's the best he can do
            return 900_000_000_000L;
        }

        if (materialEval < 0) {
            // Opponent is ahead on material, so favor the draw
            return random.NextInt64(MIN_DRAW_VALUE_WHEN_BEHIND, MAX_DRAW_VALUE_WHEN_BEHIND);
        }
        else if (materialEval > 0) {
            return -random.NextInt64(MIN_DRAW_VALUE_WHEN_BEHIND, MAX_DRAW_VALUE_WHEN_BEHIND);
        }

        return 0;
    }

    private static bool isBareKing(ulong bitboard) {
        return (bitboard & (bitboard - 1)) == 0;
    }
}

static class DictionaryExt {
    public static TValue GetOrCreate<TKey, TValue>(this IDictionary<TKey, TValue> dict, TKey key, Func<TValue> creator) 
    {
        if (!dict.TryGetValue(key, out TValue val))
        {
            val = creator();
            dict.Add(key, val);
        }

        return val;
    }
}