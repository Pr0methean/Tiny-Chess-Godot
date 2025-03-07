using System.Numerics;
using System.Runtime.InteropServices;

namespace auto_Bot_1337;

using System.Diagnostics;
using System.Linq;
using System;
using ChessChallenge.API;

public class Bot_1337 : IChessBot {
    public static int MAX_MONOTONIC_KEY = 14 | (16 << 4) | ((16 * 17) << 4);
    private const byte QUIET_DEPTH = 2;
    private const long INFINITY = 1_000_000_000_000;

    private const long MATERIAL_MULTIPLIER = 1_000_000;
    private const long VALUE_PER_AVAILABLE_MOVE = 1_000_000;
    // private const long SAME_PIECE_OPENING_MOVE_PENALTY = 1_000_000;
    private const long CHECK_PENALTY = 1_000_000_000;

    private const long FIFTY_MOVE_COUNTER_PENALTY = 100;
    private const long MIN_DRAW_VALUE_WHEN_BEHIND = 100 * 100 * 100 * FIFTY_MOVE_COUNTER_PENALTY;
    private const long MAX_DRAW_VALUE_WHEN_BEHIND = 2 * MIN_DRAW_VALUE_WHEN_BEHIND;
    // private const long CHECK_BONUS = 1_000_000;
    // private const long CASTLING_BONUS = 2_000_000;
    private static readonly long[] PIECE_VALUES = [0, 100, 305, 333, 563, 950, 0];
    private static readonly long[] PIECE_RANK_PUSH_VALUES = [0, 400, 400, 800, 1200, 1600, 400];
    private static readonly long[] PIECE_FILE_PUSH_VALUES = [0, 100, 500, 400, 600, 1200, 300];
    private static readonly long[] PIECE_SWARM_VALUES = [0, 75, 150, 300, 400, 450, 200];
    private static readonly long[] WHITE_PASSED_PAWN_VALUES = [0, 0, 0, 0, 16, 64, 128, 128];
    private static readonly long[] BLACK_PASSED_PAWN_VALUES = [128, 128, 64, 16, 0, 0, 0, 0];
    private static readonly long[] FILE_CENTER_DISTANCE_VALUES = [6, 3, 1, 0, 0, 1, 3, 6];
    private static readonly long[] WHITE_RANK_ADVANCEMENT_VALUES = [0, 3, 6, 9, 11, 13, 14, 15];
    private static readonly long[] BLACK_RANK_ADVANCEMENT_VALUES = [15, 14, 13, 11, 9, 6, 3, 0];
    private static Square[] SQUARES = Enumerable.Range(0, 64).Select(i => new Square(i)).ToArray();
    private static Random random = new();
    private const byte EXACT = 0;
    private const byte LOWERBOUND = 1;
    private const byte UPPERBOUND = 2;

    private const long BARE_KING_EVAL = 100_000_000_000L;

    // Make the struct readonly and add StructLayout attribute
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    private readonly struct CacheEntry {
        public readonly long Score;
        public readonly byte Depth;
        public readonly byte NodeType;
        public CacheEntry(long score, byte depth, byte nodeType) {
            Score = score;
            Depth = depth;
            NodeType = nodeType;
        }
    }

    private static Dictionary<ulong, long>[] materialEvalCache = new Dictionary<ulong, long>[MAX_MONOTONIC_KEY + 1];
    private static Dictionary<ulong, CacheEntry>[] alphaBetaCache = new Dictionary<ulong, CacheEntry>[MAX_MONOTONIC_KEY + 1];
    public static Dictionary<ulong, long>[] mateOrDrawCache = new Dictionary<ulong, long>[MAX_MONOTONIC_KEY + 1];
    private int prevMonotonicKey = MAX_MONOTONIC_KEY;

    static Bot_1337() {
        for (int i = 0; i <= MAX_MONOTONIC_KEY; i++) {
            materialEvalCache[i] = new Dictionary<ulong, long>();
            alphaBetaCache[i] = new Dictionary<ulong, CacheEntry>();
            mateOrDrawCache[i] = new Dictionary<ulong, long>();
        }
    }
    
    public Move Think(Board board, Timer timer) {
        // [Seb tweak start]- (adding tiny opening book for extra variety when playing against humans)
        if (board.PlyCount < 32) {
            Move bookMove = TinyOpeningBook.TryGetMove(board, randomlyDontUseBookProb: board.PlyCount / 32.0);
            if (!bookMove.IsNull) {
                return bookMove;
            }
        }

        // [Seb tweak end]
        Move bestMove = Move.NullMove;
        long bestValue = long.MinValue;
        Span<Move> legalMoves = stackalloc Move[128];
        board.GetLegalMovesNonAlloc(ref legalMoves);
        int bestMoveMonotonicKey = MAX_MONOTONIC_KEY;
        foreach (var move in legalMoves) {
            board.MakeMove(move);
            long value = -AlphaBeta(board, QUIET_DEPTH - 1, -INFINITY, INFINITY, !board.IsWhiteToMove);

            if (bestMove.IsNull || value > bestValue || (value == bestValue && random.Next(2) != 0)) {
                bestValue = value;
                bestMove = move;
                if (move.IsCapture || move.MovePieceType == PieceType.Pawn) {
                    bestMoveMonotonicKey = monotonicKey(board);
                }
            }
            board.UndoMove(move);
        }

        if (bestMoveMonotonicKey < prevMonotonicKey) {
            for (int i = bestMoveMonotonicKey + 1; i <= prevMonotonicKey; i++) {
                materialEvalCache[i].Clear();
                alphaBetaCache[i].Clear();
                mateOrDrawCache[i].Clear();
            }
            prevMonotonicKey = bestMoveMonotonicKey;
            GC.Collect();
        }

        return bestMove;
    }

    public static int monotonicKey(Board board) {
        ulong whitePawnsBitboard = board.GetPieceBitboard(PieceType.Pawn, true);
        ulong blackPawnsBitboard = board.GetPieceBitboard(PieceType.Pawn, false);
        ulong pawnsBitboard = whitePawnsBitboard | blackPawnsBitboard;
        ulong backHalfPawnsBitboard = (whitePawnsBitboard & 0xffffffff00000000) | (blackPawnsBitboard & 0x00000000ffffffff);
        int numPawns = BitOperations.PopCount(pawnsBitboard);
        int numBackHalfPawns = BitOperations.PopCount(backHalfPawnsBitboard);
        int numPiecesPromotableTo = BitOperations.PopCount(board.AllPiecesBitboard) - numPawns - 2;
        return numPiecesPromotableTo | (numPawns << 4) | ((numBackHalfPawns * 17) << 4);
    }

    private long AlphaBeta(Board board, byte quietDepth, long alpha, long beta, bool maximizingPlayer) {
        ulong key = board.ZobristKey;
        int monotonicKey = Bot_1337.monotonicKey(board);
        // Cache lookup
        if (alphaBetaCache[monotonicKey].TryGetValue(key, out var entry) && entry.Depth >= quietDepth) {
            if (entry.NodeType == EXACT) return entry.Score;
            if (entry.NodeType == LOWERBOUND) alpha = Math.Max(alpha, entry.Score);
            if (entry.NodeType == UPPERBOUND) beta = Math.Min(beta, entry.Score);
            if (alpha >= beta) return entry.Score;
        }
        long score;
        if (mateOrDrawCache[monotonicKey].TryGetValue(key, out score)) {
            goto cacheStore;
        }
        if (board.IsInsufficientMaterial() || board.IsFiftyMoveDraw()) {
            score = evaluateDraw(EvaluateMaterial(board));
            mateOrDrawCache[monotonicKey][key] = score;
            goto cacheStore;
        }

        Span<Move> legalMoves = stackalloc Move[128];
        board.GetLegalMovesNonAlloc(ref legalMoves);
        if (legalMoves.Length == 0) {
            if (board.IsInCheck()) {
                // Checkmate
                score = INFINITY * (board.IsWhiteToMove ? -1 : 1);
            } else {
                // Stalemate
                score = evaluateDraw(EvaluateMaterial(board));
            }
            mateOrDrawCache[monotonicKey][key] = score;
            goto cacheStore;
        }
        
        // Check this after GetLegalMoves, so that IsInCheck hits cache
        if (board.IsRepeatedPosition()) {
            // Don't store in mateOrDrawCache, because Board's repetition rule is 2-fold while Arbiter's is 3-fold
            long baseScore = EvaluateMaterial(board) * MATERIAL_MULTIPLIER;
            if (board.IsInCheck()) {
                baseScore += CHECK_PENALTY * (board.IsWhiteToMove ? -1 : 1);
            }
            score = evaluateDraw(baseScore);
            goto cacheStore;
        }

        bool foundNonQuietMove = false;
        score = maximizingPlayer ? -INFINITY : INFINITY;
        foreach (var move in legalMoves) {
            byte nextDepth;
            if (move.IsCapture || move.IsPromotion) {
                nextDepth = quietDepth;
                foundNonQuietMove = true;
            }
            else {
                if (quietDepth == 0) {
                    continue;
                }
                nextDepth = (byte) (quietDepth - 1);
            }
            board.MakeMove(move);
            long eval = AlphaBeta(board, nextDepth, alpha, beta, false);
            board.UndoMove(move);
            if (maximizingPlayer) {
                score = Math.Max(score, eval);
                alpha = Math.Max(alpha, eval);
            } else {
                score = Math.Min(score, eval);
                beta = Math.Min(beta, eval);
            }
            if (beta <= alpha)
                break;
        }
        if (alpha < beta && quietDepth == 0 && !foundNonQuietMove) {
            score = EvaluatePosition(board, legalMoves);
        }
        // Cache store
        cacheStore:
        var nodeType = score <= alpha ? UPPERBOUND : 
            score >= beta ? LOWERBOUND : EXACT;

        alphaBetaCache[monotonicKey][key] = new CacheEntry(
            score, quietDepth, nodeType
        );
        return score;
    }

    // Positive favors white
    private long EvaluatePosition(Board board, Span<Move> legalMoves) { 
        var evaluation = EvaluateMaterial(board) * MATERIAL_MULTIPLIER;
        bool isWhite = board.IsWhiteToMove;
        // Check penalty
        if (board.IsInCheck()) {
            evaluation -= CHECK_PENALTY * (isWhite ? 1 : -1);
        }
        
        // Swarm heuristic - bonus for pieces near enemy king
        // Push heuristic - bonus for advancing pieces toward enemy
        evaluation += CalculateSwarmAndPushBonus(board);

        // MinOpptMove heuristic - prefer to leave opponent with fewer possible responses\
        evaluation += VALUE_PER_AVAILABLE_MOVE * legalMoves.Length * (isWhite ? 1 : -1);
        if (board.TrySkipTurn()) {
            Span<Move> opponentLegalMoves = stackalloc Move[128];
            board.GetLegalMovesNonAlloc(ref opponentLegalMoves);
            evaluation -= VALUE_PER_AVAILABLE_MOVE * opponentLegalMoves.Length * (isWhite ? 1 : -1);
            board.UndoSkipTurn();
        }

        if (board.FiftyMoveCounter >= 40 && evaluation != 0) {
            long underdogMultiplier = (evaluation > 0) ? -1 : 1;
            evaluation += underdogMultiplier * FIFTY_MOVE_COUNTER_PENALTY *
                          board.FiftyMoveCounter * board.FiftyMoveCounter * board.FiftyMoveCounter;
        }
        return evaluation;
    }

    // Positive favors white. Cache shared between both sides.
    private static long EvaluateMaterial(Board board) {
        int monotonicKey = Bot_1337.monotonicKey(board);
        return materialEvalCache[monotonicKey].GetOrCreate(board.ZobristKey, () => {
            // Material and basic position evaluation
            long evaluation = 0;
            if (isBareKing(board.WhitePiecesBitboard)) {
                Debug.WriteLine("Skipping material evaluation: white is a bare king!");
                evaluation = -BARE_KING_EVAL;
            }
            else if (isBareKing(board.BlackPiecesBitboard)) {
                Debug.WriteLine("Skipping material evaluation: black is a bare king!");
                evaluation = BARE_KING_EVAL;
            }
            else {
                for (int square = 0; square < 64; square++) {
                    Piece piece = board.GetPiece(SQUARES[square]);
                    long pieceValue = PIECE_VALUES[(int)piece.PieceType]; 
                    if (piece.IsPawn) {
                        int rank = square >> 3;
                        pieceValue += (piece.IsWhite ? WHITE_PASSED_PAWN_VALUES : BLACK_PASSED_PAWN_VALUES)
                            [rank];
                    }

                    evaluation += pieceValue * (piece.IsWhite ? 1 : -1);
                }

                Debug.WriteLine("Material eval: {0}", evaluation);
            }

            return evaluation;
        });
    }

    private long CalculateSwarmAndPushBonus(Board board) {
        long bonus = 0;
        for (int square = 0; square < 64; square++) {
            Piece piece = board.GetPiece(SQUARES[square]);
            if (!piece.IsNull) {
                long pieceBonus = 0;
                Square enemyKingSquare = board.GetKingSquare(!piece.IsWhite);
                int distance = CalculateKingDistance(square, enemyKingSquare.Index);
                pieceBonus -= distance * PIECE_SWARM_VALUES[(int)piece.PieceType];
                int rank = square >> 3;
                int file = square & 7;
                var pieceType = (int) piece.PieceType;
                if (piece.IsWhite) {
                    pieceBonus += WHITE_RANK_ADVANCEMENT_VALUES[rank] 
                                  * PIECE_RANK_PUSH_VALUES[pieceType];
                } else {
                    pieceBonus += BLACK_RANK_ADVANCEMENT_VALUES[rank]
                                  * PIECE_RANK_PUSH_VALUES[pieceType];
                }
                if (rank != (piece.IsWhite ? 0 : 7) && pieceType != (int)PieceType.Rook) {
                    pieceBonus -= FILE_CENTER_DISTANCE_VALUES[file]
                                  * PIECE_FILE_PUSH_VALUES[pieceType];
                }
                else {
                    pieceBonus -= FILE_CENTER_DISTANCE_VALUES[0]
                                  * PIECE_FILE_PUSH_VALUES[pieceType];
                }
                bonus += pieceBonus * (piece.IsWhite ? 1 : -1);
            }
        }
        
        return bonus;
    }
    
    private static long evaluateDraw(long materialEval) {
        if (materialEval >= BARE_KING_EVAL) {
            // A draw is almost as good as a win for a bare king since it's the best he can do
            return -900_000_000_000L;
        }
        if (materialEval <= -BARE_KING_EVAL) {
            // A draw is almost as good as a win for a bare king since it's the best he can do
            return 900_000_000_000L;
        }

        if (materialEval < 0) {
            // Black is ahead on material, so white favors the draw
            return random.NextInt64(MIN_DRAW_VALUE_WHEN_BEHIND, MAX_DRAW_VALUE_WHEN_BEHIND);
        }
        else if (materialEval > 0) {
            // White is ahead on material, so black favors the draw
            return -random.NextInt64(MIN_DRAW_VALUE_WHEN_BEHIND, MAX_DRAW_VALUE_WHEN_BEHIND);
        }

        return 0;
    }

    private int CalculateKingDistance(int square1, int square2) {
        int x1 = square1 & 7;
        int y1 = square1 >> 3;
        int x2 = square2 & 7;
        int y2 = square2 >> 3;
        
        return Math.Max(Math.Abs(x1 - x2), Math.Abs(y1 - y2));
    }
    
    private static bool isBareKing(ulong bitboard) {
        return (bitboard & (bitboard - 1)) == 0;
    }
}

static class DictionaryExt {
    public static TValue GetOrCreate<TKey, TValue>(this IDictionary<TKey, TValue> dict, TKey key, Func<TValue> creator) 
    {
        if (!dict.TryGetValue(key, out TValue val)) {
            val = creator();
            dict.Add(key, val);
        }

        return val;
    }
}