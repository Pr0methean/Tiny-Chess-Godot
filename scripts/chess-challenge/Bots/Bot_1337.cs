namespace auto_Bot_1337;

using System.Diagnostics;
using System.Linq;
using System;
using ChessChallenge.API;

public class Bot_1337 : IChessBot {
    private const long MY_PIECE_VALUE_MULTIPLIER = 900_000;
    private const long ENEMY_PIECE_VALUE_MULTIPLIER = 1_000_000;
    private const long MY_PIECE_VALUE_PER_CAPTURING_MOVE_MULTIPLIER = 20_000;
    private const long PENALTY_PER_ENEMY_MOVE = 1_000_000;
    private const long MIN_DRAW_VALUE_WHEN_BEHIND = 0;
    private const long MAX_DRAW_VALUE_WHEN_BEHIND = 900_000_000;
    private const long MAX_MOVE_VALUE_NOISE = 1_000;
    private const long SAME_PIECE_OPENING_MOVE_PENALTY = 1_000_000;
    private const long ENEMY_CHECK_BONUS = 1_000_000_000;
    private const long CHECK_BONUS = 1_000_000;
    private const long CASTLING_BONUS = 2_000_000;
    private static readonly long[] PIECE_VALUES = [0, 100, 305, 333, 563, 950, 20_000];
    private static readonly long[] PIECE_RANK_PUSH_VALUES = [0, 400, 400, 800, 1200, 1600, 400];
    private static readonly long[] PIECE_FILE_PUSH_VALUES = [0, 100, 500, 400, 600, 1200, 300];
    private static readonly long[] PIECE_SWARM_VALUES = [0, 75, 150, 300, 400, 450, 200];
    private static readonly long[] WHITE_PASSED_PAWN_VALUES = [0, 0, 0, 0, 16, 64, 128, 0];
    private static readonly long[] BLACK_PASSED_PAWN_VALUES = [0, 128, 64, 16, 0, 0, 0, 0];
    private static readonly long[] FILE_CENTER_DISTANCE_VALUES = [6, 3, 1, 0, 0, 1, 3, 6];
    private static readonly long[] WHITE_RANK_ADVANCEMENT_VALUES = [0, 3, 6, 9, 11, 13, 14, 15];
    private static readonly long[] BLACK_RANK_ADVANCEMENT_VALUES = [15, 14, 13, 11, 9, 6, 3, 0];
    private static Random random = new();

    public struct CacheableBoardState {
        public long materialEval;
        public Move[] legalMoves;
        public long? mateOrDrawEval;
    }
    
    private static Dictionary<ulong,  CacheableBoardState> cacheableBoardStateZobrist = new();
    private static Dictionary<ulong, Dictionary<ushort, ulong>> moveResults = new();
    private Dictionary<(ushort, ulong), long> shortEvalCache = new();
    private Dictionary<(ulong, ulong), long> longEvalCache = new();

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
        CacheableBoardState boardState = getCacheableState(board);
        long fiftyMoveResetValue = 0;
        long fiftyMoveCounter = board.FiftyMoveCounter;
        if (boardState.materialEval > 0 && fiftyMoveCounter >= 60) {
            // Player that's ahead wants to reset the 50-move-rule counter.
            fiftyMoveResetValue = fiftyMoveCounter * fiftyMoveCounter * fiftyMoveCounter * 10_000;
            Debug.WriteLine("Fifty-move reset value: {0}", fiftyMoveResetValue);
        }

        var minOpptMovesBaseline = getMinOpptMovesBaseline(board);
        long bestScore = long.MinValue;
        Move? bestMove = null;
        foreach (Move move in boardState.legalMoves) {
            board.MakeMove(move);
            // Repeated positions don't factor into the Zobrist hash, but the API treats them as draws (even if they've
            // only occurred once before)
            long score;
            if (board.IsRepeatedPosition() || board.IsFiftyMoveDraw()) {
                score = evaluateDraw(boardState.materialEval);
            } else {
                CacheableBoardState boardStateAfterMove = getCacheableState(board);
                score = moveScoreZobrist.GetOrCreate(board.ZobristKey | (UInt128)move.RawValue << 64,
                    () => {
                        var mateOrDraw = boardStateAfterMove.mateOrDrawEval;
                        if (mateOrDraw != null) {
                            return (long)mateOrDraw;
                        }
                        Debug.WriteLine("Evaluating {0}", move);
                        long bestResponseScoreRelBaseline = -responseScoreZobrist.GetOrCreate(board.ZobristKey, () => {
                            Debug.WriteLine("Responses: {0}", boardStateAfterMove.legalMoves.Length);
                            long bestResponseScore = long.MinValue;
                            foreach (var response in boardStateAfterMove.legalMoves) {
                                var responseScore = ShortEvalMove(board, response, 1);
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
                            return bestResponseScore;
                        }) + minOpptMovesScore(boardStateAfterMove.legalMoves) - minOpptMovesBaseline;
                        Debug.WriteLine("Score based on responses: {0}", bestResponseScoreRelBaseline);
                        if (move.IsCapture) {
                            var capture_bonus = evalCaptureBonus(board, move, iAmWhite, ENEMY_PIECE_VALUE_MULTIPLIER);
                            Debug.WriteLine("Capture bonus: {0}", capture_bonus);
                            bestResponseScoreRelBaseline += capture_bonus;
                        }

                        if (iAmABareKing) {
                            goto scoreFinishedExceptNoise;
                        }

                        if (board.IsInCheck()) {
                            Debug.WriteLine("Check bonus: {0}", CHECK_BONUS);
                            bestResponseScoreRelBaseline += CHECK_BONUS;
                        }

                        if (move.IsCastles) {
                            Debug.WriteLine("Castling bonus: {0}", CASTLING_BONUS);
                            bestResponseScoreRelBaseline += CASTLING_BONUS;
                            // Push/swarm logic won't handle castling well, so skip it
                            goto scoreFinishedExceptNoise;
                        }

                        if (board.PlyCount < 10 && (move.MovePieceType is PieceType.Bishop or PieceType.Rook or PieceType.Queen
                                                    || move is { MovePieceType: PieceType.Knight, TargetSquare.Rank: 0 or 7 })
                                                && move.StartSquare.Rank != 0 && move.StartSquare.Rank != 7) {
                            Debug.WriteLine("Same piece opening move penalty: {0}", SAME_PIECE_OPENING_MOVE_PENALTY);
                            bestResponseScoreRelBaseline -= SAME_PIECE_OPENING_MOVE_PENALTY;
                        }

                        PieceType pushingPiece = move.MovePieceType;
                        if (move.IsPromotion) {
                            bestResponseScoreRelBaseline += MY_PIECE_VALUE_MULTIPLIER * PIECE_VALUES[(int)move.PromotionPieceType];
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
                        bestResponseScoreRelBaseline += PIECE_RANK_PUSH_VALUES[(int)pushingPiece] * rankPushAdjustment
                                  + PIECE_FILE_PUSH_VALUES[(int)pushingPiece] * filePushAdjustment
                                  + PIECE_SWARM_VALUES[(int)pushingPiece] * swarmAdjustment;
                        scoreFinishedExceptNoise:
                        Debug.WriteLine("Score before baselining and noise: {0}", bestResponseScoreRelBaseline);
                        // score -= baseline;
                        return bestResponseScoreRelBaseline;
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

    private long ShortEvalMove(Board boardBeforeMove, Move move, uint depth) {
        long keyAfterMove = moveResults.GetOrCreate((boardBeforeMove.ZobristKey, (ushort)move.RawValue),
            () => {
                boardBeforeMove.MakeMove(move);
                ulong key = boardBeforeMove.ZobristKey;
                boardBeforeMove.UndoMove(move);
                return key;
            });
        long cachedResponse;
        if (longEvalCache.TryGetValue((move.RawValue, keyAfterMove), out cachedResponse)) {
            return cachedResponse;
        }
        return shortEvalCache.GetOrCreate((move.RawValue, keyAfterMove), () => {
            bool iAmWhite = boardBeforeMove.IsWhiteToMove;
            boardBeforeMove.MakeMove(move);
            var boardStateAfterResponse = getCacheableState(boardBeforeMove);
            var mateOrDrawInResponse = boardStateAfterResponse.mateOrDrawEval;
            if (mateOrDrawInResponse != null) {
                return (long)(mateOrDrawInResponse);
            }

            long responseCaptureBonus = evalCaptureBonus(boardBeforeMove, move, !iAmWhite, MY_PIECE_VALUE_MULTIPLIER);
            long responseCheckBonus = boardBeforeMove.IsInCheck() ? ENEMY_CHECK_BONUS : 0;
            long responsePromotionBonus = move.IsPromotion
                ? PIECE_VALUES[(int)move.PromotionPieceType] * ENEMY_PIECE_VALUE_MULTIPLIER
                : 0;
            long responseScore = responseCaptureBonus + responseCheckBonus + responsePromotionBonus;
            if (depth == 0) return responseScore;
            long bestResponseToResponseScore = long.MinValue;
            foreach (var responseToResponse in boardStateAfterResponse.legalMoves) {
                long responseScore;
                if (!shortEvalCache.TryGetValue((responseToResponse.RawValue, boardBeforeMove
                        out responseScore) && depth >= 1) {
                    responseScore = ShortEvalMove(boardBeforeMove, responseToResponse, depth - 1);
                }

                if (responseScore >= 1_000_000_000_000) {
                    bestResponseToResponseScore = responseScore;
                    break;
                }

                if (responseScore > bestResponseToResponseScore) {
                    bestResponseToResponseScore = responseScore;
                }
            }

            return bestResponseToResponseScore;
        });
    }

    private long LongEvalMove(Board board, Move response, uint longDepth, uint shortDepth) {
        
    }

    private long getMinOpptMovesBaseline(Board board) {
        board.MakeMove(Move.NullMove);
        // Prevents a bias against previously depth-1-evaluated moves when considering them as responses to responses
        long minOpptMovesBaseline = minOpptMovesScore(getCacheableState(board).legalMoves);
        board.UndoMove(Move.NullMove);
        return minOpptMovesBaseline;
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

    public CacheableBoardState getCacheableState(Board board) {
        return cacheableBoardStateZobrist.GetOrCreate(board.ZobristKey, () => {
            long materialEval;
            if (isBareKing(board.WhitePiecesBitboard)) {
                Debug.WriteLine("Skipping material evaluation: white is a bare king!");
                materialEval = 100_000_000_000L;
            }
            else {
                if (isBareKing(board.BlackPiecesBitboard)) {
                    Debug.WriteLine("Skipping material evaluation: black is a bare king!");
                    materialEval = -100_000_000_000L;
                }
                else {
                    long materialEval1 = 0;
                    for (int i = 0; i < 64; i++) {
                        Square square = new Square(i);
                        Piece piece = board.GetPiece(square);
                        bool isWhite = piece.IsWhite;
                        materialEval1 += PIECE_VALUES[(int)piece.PieceType] * (isWhite ? -1 : 1);
                        if (piece.IsPawn) {
                            materialEval1 += (isWhite ? WHITE_PASSED_PAWN_VALUES : BLACK_PASSED_PAWN_VALUES)
                                [square.Rank] * (isWhite ? -1 : 1);
                        }
                    }

                    Debug.WriteLine("Material eval: {0}", materialEval1);
                    materialEval = materialEval1;
                }
            }

            Move[] legalMoves = board.GetLegalMoves();
            long? mateOrDrawEval;
            if (legalMoves.Length == 0) {
                if (board.IsInCheck()) {
                    // Checkmate
                    mateOrDrawEval = 1_000_000_000_000L;
                }
                else {
                    // Stalemate
                    mateOrDrawEval = evaluateDraw(materialEval);
                }
            }
            else {
                if (board.IsInsufficientMaterial()) {
                    mateOrDrawEval = 0;
                } else {
                    mateOrDrawEval = null;
                }
            }

            return new CacheableBoardState {
                legalMoves = legalMoves,
                mateOrDrawEval = mateOrDrawEval,
                materialEval = materialEval
            };
        });
    }

    private static long evaluateDraw(long materialEval) {
        if (materialEval >= 100_000_000_000) {
            // A draw is almost as good as a win for a bare king since it's the best he can do
            return 900_000_000_000L;
        }
        if (materialEval <= -100_000_000_000) {
            // A draw is almost as good as a win for a bare king since it's the best he can do
            return -900_000_000_000L;
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