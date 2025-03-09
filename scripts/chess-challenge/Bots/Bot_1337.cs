using System.Numerics;
using System.Runtime.InteropServices;

namespace auto_Bot_1337;

using System.Diagnostics;
using System;
using ChessChallenge.API;

public class Bot_1337 : IChessBot {
    public static int MAX_MONOTONIC_KEY = (63 * 510) + (63 * 510 + 1) * (24 + 25 * 22);
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
    private const long BARE_KING_EVAL = 100_000_000_000L;
    private static bool firstNonBookMove = false;

    // Make the struct readonly and add StructLayout attribute
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public readonly record struct CacheEntry {
        public long LowerBound { get; }
        public long UpperBound { get; }
        public byte QuietDepth { get; }
        public byte TotalDepth { get; }

        public CacheEntry(long lowerBound, long upperBound, byte quietDepth, byte totalDepth) {
            // Add validation
            if (totalDepth < quietDepth)
                throw new ArgumentException("Total depth must be >= quiet depth");
            if (upperBound < lowerBound)
                throw new ArgumentException("Upper bound must be >= lower bound");

            QuietDepth = quietDepth;
            TotalDepth = totalDepth;
            LowerBound = lowerBound;
            UpperBound = upperBound;
        }
    }

    public static Dictionary<ulong, CacheEntry>?[] alphaBetaCache = new Dictionary<ulong, CacheEntry>?[MAX_MONOTONIC_KEY + 1];
    private static int currentMonotonicKey = MAX_MONOTONIC_KEY;

    public Move Think(Board board, Timer timer) {
        // [Seb tweak start]- (adding tiny opening book for extra variety when playing against humans)
        if (board.PlyCount < 32) {
            Move bookMove = TinyOpeningBook.TryGetMove(board, randomlyDontUseBookProb: board.PlyCount / 32.0);
            if (!bookMove.IsNull) {
                return bookMove;
            }
        }

        if (!firstNonBookMove) {
            currentMonotonicKey = monotonicKey(board);
            firstNonBookMove = true;
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
        bool deletedSomething = false;
        for (int i = newCurrentMonotonicKey + 1; i < currentMonotonicKey; i++) {
            if (alphaBetaCache[i] != null && alphaBetaCache[i].Count > 0) {
                deletedSomething = true;
            }
            alphaBetaCache[i] = null;
        }
        currentMonotonicKey = newCurrentMonotonicKey;
        if (deletedSomething) {
            GC.Collect(GC.MaxGeneration, GCCollectionMode.Aggressive, true, true);
        }
    }
    
    public static int monotonicKey(Board board) {
        ulong whitePawnBitboard = board.GetPieceBitboard(PieceType.Pawn, true);
        ulong blackPawnBitboard = board.GetPieceBitboard(PieceType.Pawn, false);
        ulong rank2PawnsKey = ((whitePawnBitboard & 0x0000_0000_0000_ff00) >> 8) 
                              + ((blackPawnBitboard & 0x00ff_0000_0000_0000) >> 48);
        ulong rank3PawnsKey = ((whitePawnBitboard &   0x0000_0000_00ff_0000) >> 16) 
                              + ((blackPawnBitboard & 0x0000_ff00_0000_0000) >> 40);
        ulong rank4PawnsKey = ((whitePawnBitboard &   0x0000_0000_ff00_0000) >> 24) 
                              + ((blackPawnBitboard & 0x0000_00ff_0000_0000) >> 32);
        ulong rank5PawnsKey = ((whitePawnBitboard &   0x0000_00ff_0000_0000) >> 32) 
                              + ((blackPawnBitboard & 0x0000_0000_ff00_0000) >> 24);
        ulong rank6PawnsKey = ((whitePawnBitboard &   0x0000_ff00_0000_0000) >> 40) 
                              + ((blackPawnBitboard & 0x0000_0000_00ff_0000) >> 16);
        ulong rank7PawnsKey = ((whitePawnBitboard &   0x00ff_0000_0000_0000) >> 48) 
                              + ((blackPawnBitboard & 0x0000_0000_0000_ff00) >> 8);
        int pawnsKey = (int) (63 * rank2PawnsKey
                       + 31 * rank3PawnsKey
                       + 15 * rank4PawnsKey
                       + 7 * rank5PawnsKey
                       + 3 * rank6PawnsKey
                       + rank7PawnsKey);
        ulong bishopBitboard = board.GetPieceBitboard(PieceType.Bishop, false)
                               | board.GetPieceBitboard(PieceType.Bishop, true);
        ulong lightBishopBitboard = bishopBitboard & 0x55aa_55aa_55aa_55aa;
        ulong darkBishopBitboard = bishopBitboard & 0xaa55_aa55_aa55_aa55;
        int totalPawns = BitOperations.PopCount(whitePawnBitboard) + BitOperations.PopCount(blackPawnBitboard);
        int totalPawnsLightBishopsKnightsAndQueens = BitOperations.PopCount(board.GetPieceBitboard(PieceType.Queen, false))
                                  + BitOperations.PopCount(board.GetPieceBitboard(PieceType.Queen, true))
                                  + BitOperations.PopCount(lightBishopBitboard)
                                  + BitOperations.PopCount(board.GetPieceBitboard(PieceType.Knight, false))
                                  + BitOperations.PopCount(board.GetPieceBitboard(PieceType.Knight, true))
                                  + totalPawns;
        int totalPawnsDarkBishopsAndRooks = BitOperations.PopCount(board.GetPieceBitboard(PieceType.Rook, false))
                                 + BitOperations.PopCount(board.GetPieceBitboard(PieceType.Rook, true))
                                 + BitOperations.PopCount(darkBishopBitboard)
                                 + totalPawns;
        int nonKingPiecesKey = totalPawnsLightBishopsKnightsAndQueens + 25 * totalPawnsDarkBishopsAndRooks;
        return pawnsKey + (63 * 510 + 1) * nonKingPiecesKey;
    }

    public static CacheEntry? getAlphaBetaCacheEntry(Board board) {
        int key = monotonicKey(board);
        ulong zobristKey = board.ZobristKey;
        if (key > currentMonotonicKey) {
            throw new ArgumentException("Key exceeds what's already been eliminated");
        }
        if (alphaBetaCache[key] == null) {
            return null;
        }

        CacheEntry entry;
        if (alphaBetaCache[key].TryGetValue(zobristKey, out entry)) {
            return entry;
        }
        return null;
    }
    
    private static void setAlphaBetaCacheEntry(Board board, CacheEntry value) {
        int key = monotonicKey(board);
        ulong zobristKey = board.ZobristKey;
        if (key > currentMonotonicKey) {
            throw new ArgumentException("Key exceeds what's already been eliminated");
        }
        if (alphaBetaCache[key] == null) {
            alphaBetaCache[key] = new Dictionary<ulong, CacheEntry>();
        }
        alphaBetaCache[key][zobristKey] = value;
    }


    private long AlphaBeta(Board board, byte quietDepth, byte totalDepth, long alpha, long beta, bool maximizingPlayer) {
        ulong key = board.ZobristKey;
        int monotonicKey = Bot_1337.monotonicKey(board);
        // Cache lookup
        var entry = getAlphaBetaCacheEntry(board);
        if (entry is {} cacheEntry) {
            if (cacheEntry.TotalDepth >= totalDepth || cacheEntry.QuietDepth >= quietDepth) {
                alpha = Math.Max(alpha, cacheEntry.LowerBound);
                beta = Math.Min(beta, cacheEntry.UpperBound);
            }
        }
        if (alpha >= beta) {
            return maximizingPlayer ? beta : alpha;
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
                byte nextQuietDepth;
                if (isUnquietMove(move)) {
                    nextQuietDepth = (byte) Math.Min(quietDepth, totalDepth - 1);
                    foundNonQuietMove = true;
                }
                else {
                    if (quietDepth == 0) {
                        continue;
                    }
                    nextQuietDepth = (byte) (quietDepth - 1);
                }
                board.MakeMove(move);
                long eval = AlphaBeta(board, nextQuietDepth, (byte) (totalDepth - 1), alpha, beta, false);
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

        if (score < alpha) {
            score = alpha;
        } else if (score > beta) {
            score = beta;
        }
        // Cache store
        cacheStore:
        long lowerBound = (score >= beta) ? score : -INFINITY;
        long upperBound = (score <= alpha) ? score : INFINITY;
        if (storeEndgame) {
            // If a state is terminal, the path length leading to it doesn't matter
            quietDepth = byte.MaxValue;
            totalDepth = byte.MaxValue;
        } else {
            var cacheEntryToUpdate = getAlphaBetaCacheEntry(board);
            if (cacheEntryToUpdate is {} existing) {
                quietDepth = Math.Min(existing.QuietDepth, quietDepth);
                totalDepth = Math.Min(existing.TotalDepth, totalDepth);
                lowerBound = Math.Max(lowerBound, existing.LowerBound);
                upperBound = Math.Min(upperBound, existing.UpperBound);
                if (lowerBound > upperBound) {
                    if (totalDepth > existing.TotalDepth ||
                        (totalDepth == existing.TotalDepth && quietDepth > existing.QuietDepth)) {
                        // Deeper search is correct
                        lowerBound = score;
                        upperBound = score;
                    }
                    else {
                        // Don't write to cache if a deeper search contradicts us
                        return score;
                    }
                }
            }
        }
        setAlphaBetaCacheEntry(board, new CacheEntry(lowerBound, upperBound, quietDepth, totalDepth));
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
            var entry = getAlphaBetaCacheEntry(board);
            if (entry is { QuietDepth: byte.MaxValue, TotalDepth: byte.MaxValue }) {
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