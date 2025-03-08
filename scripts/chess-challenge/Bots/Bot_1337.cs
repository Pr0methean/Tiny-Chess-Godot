using System.Numerics;
using System.Runtime.InteropServices;

namespace auto_Bot_1337;

using System.Diagnostics;
using System;
using ChessChallenge.API;

public class Bot_1337 : IChessBot {
    public static int MAX_MONOTONIC_KEY = (5 * 16) + (5 * 16 + 1) * (16 + 17 * 30);
    private const byte QUIET_DEPTH = 2;
    private const byte MAX_TOTAL_DEPTH = 6;
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
    private static Random random = new();
    private const byte EXACT = 0;
    private const byte LOWERBOUND = 1;
    private const byte UPPERBOUND = 2; 
    private const long BARE_KING_EVAL = 100_000_000_000L;
    private static ulong trimmedCacheEntries = 0;
    private static int youngCollectionsAtLastTrim = 0;

    // Make the struct readonly and add StructLayout attribute
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public readonly record struct CacheEntry {
        public readonly long Score;
        public readonly byte QuietDepth;
        public readonly byte TotalDepth;
        public readonly byte NodeType;
        public CacheEntry(long score, byte quietDepth, byte totalDepth, byte nodeType) {
            Score = score;
            QuietDepth = quietDepth;
            NodeType = nodeType;
            TotalDepth = totalDepth;
        }
    }

    public static Dictionary<ulong, CacheEntry>?[] alphaBetaCache = new Dictionary<ulong, CacheEntry>?[MAX_MONOTONIC_KEY + 1];
    private static int currentMonotonicKey = MAX_MONOTONIC_KEY;

    static Bot_1337() {
        for (int i = 0; i <= MAX_MONOTONIC_KEY; i++) {
            alphaBetaCache[i] = new Dictionary<ulong, CacheEntry>();
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

        if (board.PlyCount == 32) {
            TinyOpeningBook.unload();
        }

        // [Seb tweak end]
        Move bestMove = Move.NullMove;
        long bestValue = long.MinValue;
        Span<Move> legalMoves = stackalloc Move[128];
        board.GetLegalMovesNonAlloc(ref legalMoves);
        foreach (var move in legalMoves) {
            board.MakeMove(move);
            long value = -AlphaBeta(board, (byte) (QUIET_DEPTH - (isUnquietMove(move) ? 1 : 0)), (byte) (MAX_TOTAL_DEPTH - 1), -INFINITY, INFINITY, !board.IsWhiteToMove);

            if (bestMove.IsNull || value > bestValue || (value == bestValue && random.Next(2) != 0)) {
                bestValue = value;
                bestMove = move;
            }
            board.UndoMove(move);
        }

        return bestMove;
    }

    public static void trimCache(int newCurrentMonotonicKey) {
        if (currentMonotonicKey <= newCurrentMonotonicKey) return;
        ulong newTrimmedCacheEntries = 0;
        for (int i = newCurrentMonotonicKey + 1; i < currentMonotonicKey; i++) {
            newTrimmedCacheEntries += (ulong) alphaBetaCache[i].Count;
            alphaBetaCache[i] = null;
        }
        currentMonotonicKey = newCurrentMonotonicKey;
        Debug.WriteLine("Lowering the monotonic key to {0} freed {1} cache entries",
            newCurrentMonotonicKey, newTrimmedCacheEntries);
        if (newTrimmedCacheEntries == 0) {
            return;
        }
        /*
        int newYoungCollectionsAtLastTrim = GC.CollectionCount(0);
        if (newYoungCollectionsAtLastTrim > youngCollectionsAtLastTrim) {
            youngCollectionsAtLastTrim = newYoungCollectionsAtLastTrim;
            trimmedCacheEntries = 0;
        }
        */
        trimmedCacheEntries += newTrimmedCacheEntries;
        // In alphaBetaCache, each entry should occupy: 
        // - 4~12 bytes for object header
        // - 8 bytes for ulong key
        // - 12 bytes for value
        // -> 24~32 bytes total
        // so each manual GC should free at least ~768 MiB (3 << 28) bytes
        // which should make a difference on my laptop, since it has 32 GiB and 12 CPU cores 
        const ulong entriesToDropBeforeManualGc = 1 << 24;
        if (trimmedCacheEntries >= entriesToDropBeforeManualGc) {
            GC.Collect(GC.MaxGeneration, 
                GCCollectionMode.Aggressive, 
                true, 
                true);
            trimmedCacheEntries = 0;
        }
    }
    
    public static int monotonicKey(Board board) {
        ulong whitePawnBitboard = board.GetPieceBitboard(PieceType.Pawn, true);
        ulong blackPawnBitboard = board.GetPieceBitboard(PieceType.Pawn, false);
        int totalPawns = BitOperations.PopCount(whitePawnBitboard | blackPawnBitboard);
        ulong rank2PawnsBitboard = (whitePawnBitboard &   0x0000_0000_0000_ff00)
                                   | (blackPawnBitboard & 0x00ff_0000_0000_0000);
        ulong rank3PawnsBitboard = (whitePawnBitboard &   0x0000_0000_00ff_0000)
                                   | (blackPawnBitboard & 0x0000_ff00_0000_0000);
        ulong rank4PawnsBitboard = (whitePawnBitboard &   0x0000_0000_ff00_0000)
                                   | (blackPawnBitboard & 0x0000_00ff_0000_0000);
        ulong rank5PawnsBitboard = (whitePawnBitboard &   0x0000_00ff_0000_0000)
                                   | (blackPawnBitboard & 0x0000_0000_ff00_0000);
        ulong rank6PawnsBitboard = (whitePawnBitboard &   0x0000_ff00_0000_0000)
                                   | (blackPawnBitboard & 0x0000_0000_00ff_0000);
        int pawnsKey = 5 * BitOperations.PopCount(rank2PawnsBitboard)
            + 4 * BitOperations.PopCount(rank3PawnsBitboard)
            + 3 * BitOperations.PopCount(rank4PawnsBitboard)
            + 2 * BitOperations.PopCount(rank5PawnsBitboard)
            + 1 * BitOperations.PopCount(rank6PawnsBitboard);
        int nonKingPiecesTotal = BitOperations.PopCount(board.AllPiecesBitboard) - 2;
        return pawnsKey + (5 * 16 + 1) * (totalPawns + 17 * nonKingPiecesTotal);
    }

    private long AlphaBeta(Board board, byte quietDepth, byte totalDepth, long alpha, long beta, bool maximizingPlayer) {
        ulong key = board.ZobristKey;
        int monotonicKey = Bot_1337.monotonicKey(board);
        // Cache lookup
        if (alphaBetaCache[monotonicKey].TryGetValue(key, out var entry) && (entry.TotalDepth >= totalDepth || entry.QuietDepth >= quietDepth )) {
            if (entry.NodeType == EXACT) return entry.Score;
            if (entry.NodeType == LOWERBOUND) alpha = Math.Max(alpha, entry.Score);
            if (entry.NodeType == UPPERBOUND) beta = Math.Min(beta, entry.Score);
            if (alpha >= beta) return entry.Score;
        }
        long score;
        bool storeEndgame = false;
        if (board.IsInsufficientMaterial() || board.IsFiftyMoveDraw()) {
            score = evaluateDraw(EvaluateMaterial(board));
            storeEndgame = true;
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
            storeEndgame = true;
            goto cacheStore;
        }
        
        // Check this after GetLegalMoves, so that IsInCheck hits cache
        if (board.IsRepeatedPosition()) {
            long baseScore = EvaluateMaterial(board) * MATERIAL_MULTIPLIER;
            if (board.IsInCheck()) {
                baseScore += CHECK_PENALTY * (board.IsWhiteToMove ? -1 : 1);
            }
            score = evaluateDraw(baseScore);
            storeEndgame = true;
            goto cacheStore;
        }

        bool foundNonQuietMove = false;
        score = maximizingPlayer ? -INFINITY : INFINITY;
        if (totalDepth > 0) {
            foreach (var move in legalMoves) {
                byte nextDepth;
                if (isUnquietMove(move)) {
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
                long eval = AlphaBeta(board, nextDepth, (byte) (totalDepth - 1), alpha, beta, false);
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
        }
        if (alpha < beta && (totalDepth == 0 || (quietDepth == 0 && !foundNonQuietMove))) {
            score = EvaluatePosition(board, monotonicKey, legalMoves);
        }
        // Cache store
        cacheStore:
        var nodeType = score <= alpha ? UPPERBOUND : 
            score >= beta ? LOWERBOUND : EXACT;
        if (storeEndgame) {
            // If a state is terminal, the path length leading to it doesn't matter
            quietDepth = byte.MaxValue;
            totalDepth = byte.MaxValue;
        }

        alphaBetaCache[monotonicKey][key] = new CacheEntry(
            score, quietDepth, totalDepth, nodeType
        );
        return score;
    }

    private static bool isUnquietMove(Move move) {
        return move.IsCapture || move.MovePieceType == PieceType.Pawn;
    }

    // Positive favors white
    private long EvaluatePosition(Board board, int monotonicKey, Span<Move> legalMoves) { 
        var evaluation = EvaluateMaterial(board) * MATERIAL_MULTIPLIER;
        bool isWhite = board.IsWhiteToMove;
        bool isInCheck = board.IsInCheck();
        // Check penalty
        if (isInCheck) {
            evaluation -= CHECK_PENALTY * (isWhite ? 1 : -1);
        }
        
        // Swarm heuristic - bonus for pieces near enemy king
        // Push heuristic - bonus for advancing pieces toward enemy
        evaluation += CalculateSwarmAndPushBonus(board);

        // MinOpptMove heuristic - prefer to leave opponent with fewer possible responses\
        evaluation += VALUE_PER_AVAILABLE_MOVE * legalMoves.Length * (isWhite ? 1 : -1);
        if (!isInCheck) {
            board.MakeMove(Move.NullMove);
            if (!(alphaBetaCache[monotonicKey].TryGetValue(board.ZobristKey, out var entry)) &&
                    entry is { QuietDepth: byte.MaxValue, TotalDepth: byte.MaxValue }) {
                Span<Move> opponentLegalMoves = stackalloc Move[128];
                board.GetLegalMovesNonAlloc(ref opponentLegalMoves);
                evaluation -= VALUE_PER_AVAILABLE_MOVE * opponentLegalMoves.Length * (isWhite ? 1 : -1);
            }
            board.UndoMove(Move.NullMove);
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
        // Material and passed-pawn evaluation
        long evaluation = 0;
        if (isBareKing(board.WhitePiecesBitboard)) {
            //Debug.WriteLine("Skipping material evaluation: white is a bare king!");
            evaluation = -BARE_KING_EVAL;
        }
        else if (isBareKing(board.BlackPiecesBitboard)) {
            //Debug.WriteLine("Skipping material evaluation: black is a bare king!");
            evaluation = BARE_KING_EVAL;
        }
        else {
            for (int square = 0; square < 64; square++) {
                PieceType pieceType = board.GetPieceType(square);
                bool pieceIsWhite = board.IsWhitePiece(square);
                long pieceValue = PIECE_VALUES[(int)pieceType]; 
                if (pieceType == PieceType.Pawn) {
                    int rank = square >> 3;
                    pieceValue += (pieceIsWhite ? WHITE_PASSED_PAWN_VALUES : BLACK_PASSED_PAWN_VALUES)
                        [rank];
                }

                evaluation += pieceValue * (pieceIsWhite ? 1 : -1);
            }

            //Debug.WriteLine("Material eval: {0}", evaluation);
        }

        return evaluation;
    }

    private long CalculateSwarmAndPushBonus(Board board) {
        long bonus = 0;
        for (int square = 0; square < 64; square++) {
            PieceType pieceType = board.GetPieceType(square);
            bool pieceIsWhite = board.IsWhitePiece(square);
            if (pieceType != PieceType.None) {
                long pieceBonus = 0;
                Square enemyKingSquare = board.GetKingSquare(!pieceIsWhite);
                int distance = CalculateKingDistance(square, enemyKingSquare.Index);
                pieceBonus -= distance * PIECE_SWARM_VALUES[(int)pieceType];
                int rank = square >> 3;
                int file = square & 7;
                pieceBonus += (pieceIsWhite ? WHITE_RANK_ADVANCEMENT_VALUES : BLACK_RANK_ADVANCEMENT_VALUES)[rank]
                    * PIECE_RANK_PUSH_VALUES[(int)pieceType];
                if (rank != (pieceIsWhite ? 0 : 7) && pieceType != PieceType.Rook) {
                    pieceBonus -= FILE_CENTER_DISTANCE_VALUES[file]
                                  * PIECE_FILE_PUSH_VALUES[(int)pieceType];
                }
                else {
                    pieceBonus -= FILE_CENTER_DISTANCE_VALUES[0]
                                  * PIECE_FILE_PUSH_VALUES[(int)pieceType];
                }
                bonus += pieceBonus * (pieceIsWhite ? 1 : -1);
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