using System.Diagnostics;
using static ChessChallenge.API.PieceType;
using static System.Numerics.BitOperations;
using System.Runtime.InteropServices;

namespace auto_Bot_1337;

using System;
using ChessChallenge.API;

public class Bot_1337 : IChessBot {
    private const uint MAX_MONOTONIC_KEY = 2 * 16807U * 65535U 
                                           + 3 * (((((((((9 * 5 + 9) * 5 + 1) * 5 + 1) * 5 + 1) * 5 + 1) * 11 + 2) * 11 + 2) * 11 + 2) * 11 + 2)
                                           + 127 * (1 << 19 - 1) * 15U;
    private const byte QUIET_DEPTH = 2;
    private const byte MAX_TOTAL_DEPTH = 6;
    private const long INFINITY = 1_000_000_000_000;
    private const int MAX_NUMBER_LEGAL_MOVES = 218; // R6R/3Q4/1Q4Q1/4Q3/2Q4Q/Q4Q2/pp1Q4/kBNN1KB1 w - - 0 1

    private const long MATERIAL_MULTIPLIER = 1_000_000;
    private const long VALUE_PER_AVAILABLE_MOVE = 500_000;
    // private const long SAME_PIECE_OPENING_MOVE_PENALTY = 1_000_000;
    private const long CHECK_PENALTY = 75_000_000;

    private const long FIFTY_MOVE_COUNTER_PENALTY = 100;
    private const long DRAW_VALUE_WHEN_BEHIND = 100 * 100 * 100 * FIFTY_MOVE_COUNTER_PENALTY;
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
    public static bool firstNonBookMove = false;

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

    private static readonly SortedDictionary<uint, Dictionary<ulong, CacheEntry>> alphaBetaCache = new();
    private static uint currentMonotonicKey = MAX_MONOTONIC_KEY;
    private static Dictionary<ulong, CacheEntry>? currentKeyCache;

    private static void sortLegalMovesPromisingFirst(ref Span<Move> moves) {
        random.Shuffle(moves);
        moves.Sort((a, b) => {
            int promotionComparison = -a.PromotionPieceType.CompareTo(b.PromotionPieceType);
            return promotionComparison != 0 ? promotionComparison : -a.CapturePieceType.CompareTo(b.CapturePieceType);  
        });
    }

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
        Span<Move> legalMoves = stackalloc Move[MAX_NUMBER_LEGAL_MOVES];
        board.GetLegalMovesNonAlloc(ref legalMoves);
        sortLegalMovesPromisingFirst(ref legalMoves);
        foreach (var move in legalMoves) {
            bool unquiet = isUnquietMove(move);
            board.MakeMove(move);
            long value = -AlphaBeta(board, (byte) (QUIET_DEPTH - (unquiet ? 1 : 0)), MAX_TOTAL_DEPTH - 1, -INFINITY, INFINITY, !board.IsWhiteToMove, Bot_1337.monotonicKey(board), unquiet);
            board.UndoMove(move);
            if (value >= INFINITY) {
                return move;
            }
            if (value != 0 && board.FiftyMoveCounter >= 40) {
                long fiftyMoveAdjustment = FIFTY_MOVE_COUNTER_PENALTY * board.FiftyMoveCounter *
                                           board.FiftyMoveCounter * board.FiftyMoveCounter;
                long underdogMultiplier = (value > 0) ? -1 : 1;
                value += underdogMultiplier * fiftyMoveAdjustment;
            }
            if (bestMove.IsNull || value > bestValue || (value == bestValue && random.Next(2) != 0)) {
                bestValue = value;
                bestMove = move;
            }
        }

        return bestMove;
    }

    public static void trimCache(Board board) {
        if (!firstNonBookMove) return;
        uint newCurrentMonotonicKey = monotonicKey(board);
        Debug.WriteLine("New monotonic key is {}", newCurrentMonotonicKey);
        if (currentMonotonicKey <= newCurrentMonotonicKey) return;
        currentMonotonicKey = newCurrentMonotonicKey;
        alphaBetaCache.Keys.SkipWhile(k => k <= newCurrentMonotonicKey)
            .ToList().ForEach(k => alphaBetaCache.Remove(k));
        currentKeyCache = null;
        alphaBetaCache.TryGetValue(currentMonotonicKey, out currentKeyCache);
    }

    public static uint monotonicKey(Board board) {
        ulong whitePawnBitboard = board.GetPieceBitboard(Pawn, true);
        ulong blackPawnBitboard = board.GetPieceBitboard(Pawn, false);
        ulong rank2PawnsKey = ((whitePawnBitboard & 0x0000_0000_0000_ff00) >> 8) 
                              | ((blackPawnBitboard & 0x00ff_0000_0000_0000) >> 40);
        ulong rank3PawnsKey = ((whitePawnBitboard &   0x0000_0000_00ff_0000) >> 16) 
                              | ((blackPawnBitboard & 0x0000_ff00_0000_0000) >> 32);
        ulong rank4PawnsKey = ((whitePawnBitboard &   0x0000_0000_ff00_0000) >> 24) 
                              | ((blackPawnBitboard & 0x0000_00ff_0000_0000) >> 24);
        ulong rank5PawnsKey = ((whitePawnBitboard &   0x0000_00ff_0000_0000) >> 32) 
                              | ((blackPawnBitboard & 0x0000_0000_ff00_0000) >> 16);
        ulong rank6PawnsKey = ((whitePawnBitboard &   0x0000_ff00_0000_0000) >> 40) 
                              | ((blackPawnBitboard & 0x0000_0000_00ff_0000) >> 8);
        ulong rank7PawnsKey = ((whitePawnBitboard &   0x00ff_0000_0000_0000) >> 48) 
                              | ((blackPawnBitboard & 0x0000_0000_0000_ff00) >> 0);
        uint pawnsKey = (uint) (16807 * rank2PawnsKey
                       + 2401 * rank3PawnsKey
                       + 343 * rank4PawnsKey
                       + 49 * rank5PawnsKey
                       + 7 * rank6PawnsKey
                       + rank7PawnsKey);
        ulong blackBishopBitboard = board.GetPieceBitboard(Bishop, false);
        ulong whiteBishopBitboard = board.GetPieceBitboard(Bishop, true);
        const ulong LIGHT_SQUARES = 0x55aa_55aa_55aa_55aa;
        const ulong DARK_SQUARES = ~LIGHT_SQUARES;
        uint blackPawnCount = (uint) PopCount(blackPawnBitboard);
        uint blackPawnsAndDarkBishops = blackPawnCount + (uint) PopCount(blackBishopBitboard & DARK_SQUARES);
        uint blackLightBishops = (uint)PopCount(blackBishopBitboard & LIGHT_SQUARES);
        uint blackKnights = (uint) PopCount(board.GetPieceBitboard(Knight, false));
        uint blackRooks = (uint) PopCount(board.GetPieceBitboard(Rook, false));
        uint blackQueens = (uint) PopCount(board.GetPieceBitboard(Queen, false));
        uint whitePawnCount = (uint) PopCount(whitePawnBitboard);
        uint whitePawnsAndLightBishops = whitePawnCount + (uint)PopCount(whiteBishopBitboard & LIGHT_SQUARES);
        uint whiteDarkBishops = (uint) PopCount(whiteBishopBitboard & DARK_SQUARES);
        uint whiteKnights = (uint) PopCount(board.GetPieceBitboard(Knight, true));
        uint whiteRooks = (uint) PopCount(board.GetPieceBitboard(Rook, true));
        uint whiteQueens = (uint) PopCount(board.GetPieceBitboard(Queen, true));
        uint nonKingPiecesKey = ((((((((blackPawnsAndDarkBishops * 5
                                        + whitePawnsAndLightBishops) * 5
                                        + blackLightBishops) * 5
                                        + whiteDarkBishops) * 5
                                      + blackQueens) * 5
                                    + whiteQueens) * 11
                                   + blackKnights) * 11
                                  + blackRooks) * 11
                                 + whiteKnights) * 11
                                + whiteRooks;
        uint castlingKey = (uint) ((board.HasQueensideCastleRight(true) ? 1 : 0)
                                                                   | (board.HasKingsideCastleRight(false) ? 2 : 0)
                                                                   | (board.HasKingsideCastleRight(true) ? 4 : 0)
                                                                   | (board.HasQueensideCastleRight(false) ? 8 : 0));
        return 2 * pawnsKey + 3 * nonKingPiecesKey + 127 * (1<<19 - 1) * castlingKey;
    }

    public static CacheEntry? getAlphaBetaCacheEntry(uint monotonicKey, Board board) {
        if (monotonicKey > currentMonotonicKey) {
            throw new ArgumentException("Key exceeds what's already been eliminated");
        }
        Dictionary<ulong, CacheEntry>? cacheForMonotonicKey;
        if (monotonicKey == currentMonotonicKey) {
            cacheForMonotonicKey = currentKeyCache;
        }
        else {
            alphaBetaCache.TryGetValue(monotonicKey, out cacheForMonotonicKey);
        }
        if (cacheForMonotonicKey == null) {
            return null;
        }
        ulong zobristKey = board.ZobristKey;
        if (cacheForMonotonicKey.TryGetValue(zobristKey, out var entry)) {
            return entry;
        }
        return null;
    }
    
    private static void setAlphaBetaCacheEntry(uint monotonicKey, Board board, CacheEntry value) {
        ulong zobristKey = board.ZobristKey;
        if (monotonicKey > currentMonotonicKey) {
            throw new ArgumentException("Key exceeds what's already been eliminated");
        }
        if (monotonicKey != currentMonotonicKey) {
            var newCache = new Dictionary<ulong, CacheEntry>();
            if (alphaBetaCache.TryAdd(monotonicKey, newCache)) {
                newCache[zobristKey] = value;
            } else {
                alphaBetaCache[monotonicKey][zobristKey] = value;
            }
        } else if (currentKeyCache == null) {
            currentKeyCache = new Dictionary<ulong, CacheEntry>();
            currentKeyCache[zobristKey] = value;
            alphaBetaCache.Add(monotonicKey, currentKeyCache);
        }
    }


    private long AlphaBeta(Board board, byte quietDepth, byte totalDepth, long alpha, long beta, bool maximizingPlayer,
        uint monotonicKey, bool previousMoveWasUnquiet) {
        long score;
        bool storeEndgame = false;
        bool isInCheck = board.IsInCheck();
        if (board.IsFiftyMoveDraw()) {
            // Zobrist key doesn't consider repetition or 50-move rule, so they may invalidate the cache
            score = evaluateDraw(EvaluateMaterial(board));
            storeEndgame = true;
            goto cacheStore;
        }
        if (board.IsRepeatedPosition()) {
            long baseScore = EvaluateMaterial(board);
            if (isInCheck) {
                baseScore += (CHECK_PENALTY / MATERIAL_MULTIPLIER) * (board.IsWhiteToMove ? -1 : 1);
            }
            score = evaluateDraw(baseScore);
            storeEndgame = true;
            goto cacheStore;
        }
        var entry = getAlphaBetaCacheEntry(monotonicKey, board);
        if (entry is {} cacheEntry) {
            // For positions after unquiet moves, use cached bounds more conservatively
            if (previousMoveWasUnquiet || isInCheck) {
                // Only use definitive cutoffs from cached bounds
                if (cacheEntry.LowerBound >= beta) {
                    return beta;  // Safe beta cutoff
                }
                if (cacheEntry.UpperBound <= alpha) {
                    return alpha; // Safe alpha cutoff
                }
                // Don't update intermediate bounds after unquiet moves
                return alpha;
            }
            
            // For quiet positions, use full cached information
            if (cacheEntry.LowerBound >= beta) {
                return cacheEntry.LowerBound;
            }
            if (cacheEntry.UpperBound <= alpha) {
                return cacheEntry.UpperBound;
            }
            alpha = Math.Max(alpha, cacheEntry.LowerBound);
            beta = Math.Min(beta, cacheEntry.UpperBound);
        }

        Span<Move> legalMoves = stackalloc Move[MAX_NUMBER_LEGAL_MOVES];
        board.GetLegalMovesNonAlloc(ref legalMoves);
        if (legalMoves.Length == 0) {
            if (isInCheck) {
                // Player to move is checkmated
                score = INFINITY * (board.IsWhiteToMove ? -1 : 1);
            } else {
                // Stalemate
                score = evaluateDraw(EvaluateMaterial(board));
            }
            storeEndgame = true;
            goto cacheStore;
        }
        bool foundNonQuietMove = false;
        score = maximizingPlayer ? -INFINITY : INFINITY;
        
        // Sort descending by promoted piece type, then by captured piece type
        sortLegalMovesPromisingFirst(ref legalMoves);
        
        if (totalDepth > 0) {
            foreach (var move in legalMoves) {
                byte nextQuietDepth;
                bool unquiet = isUnquietMove(move);
                if (unquiet) {
                    nextQuietDepth = (byte) Math.Min(quietDepth, totalDepth - 1);
                    foundNonQuietMove = true;
                }
                else {
                    if (quietDepth == 0) {
                        continue;
                    }
                    nextQuietDepth = (byte) Math.Min(quietDepth - 1, totalDepth - 1);
                }
                board.MakeMove(move);
                long eval = AlphaBeta(board, nextQuietDepth, (byte) (totalDepth - 1), alpha, beta, !maximizingPlayer, Bot_1337.monotonicKey(board), unquiet);
                board.UndoMove(move);
                if (maximizingPlayer) {
                    score = Math.Max(score, eval);
                    alpha = Math.Max(alpha, score);
                } else {
                    score = Math.Min(score, eval);
                    beta = Math.Min(beta, score);
                }
                if (beta <= alpha)
                    break;
            }
        }
        if (alpha < beta && (totalDepth == 0 || (quietDepth == 0 && !foundNonQuietMove))) {
            // We've reached our maximum depth
            if (board.IsInsufficientMaterial()) {
                score = evaluateDraw(EvaluateMaterial(board));
                storeEndgame = true;
                goto cacheStore;
            }
            score = EvaluatePosition(monotonicKey, board, legalMoves);
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
            if (lowerBound == -INFINITY && upperBound == INFINITY) {
                // No information to store
                return score;
            }
            var cacheEntryToUpdate = getAlphaBetaCacheEntry(monotonicKey, board);
            if (cacheEntryToUpdate is {} existing) {
                if (existing.TotalDepth < totalDepth || 
                        (existing.TotalDepth == totalDepth && existing.QuietDepth < quietDepth)) {
                    if (previousMoveWasUnquiet || isInCheck) {
                        return score;
                    }
                    // Return the appropriate bound from the cached entry; shallower search wins
                    return maximizingPlayer ? existing.LowerBound : existing.UpperBound;
                }
            }
            if (lowerBound > upperBound) {
                lowerBound = score;
                upperBound = score;
            }
            // Fall through to cache update only for new entries or when our search is shallower
        }
        setAlphaBetaCacheEntry(monotonicKey, board, new CacheEntry(lowerBound, upperBound, quietDepth, totalDepth));
        if (score < alpha) {
            score = alpha;
        } else if (score > beta) {
            score = beta;
        }
        return score;
    }

    private static bool isUnquietMove(Move move) {
        return move.IsCapture || move.MovePieceType == Pawn;
    }

    // Positive favors white
    public long EvaluatePosition(uint monotonicKey, Board board, Span<Move> legalMoves) { 
        var evaluation = EvaluateMaterial(board) * MATERIAL_MULTIPLIER;
        bool isWhite = board.IsWhiteToMove;
        bool isInCheck = board.IsInCheck();
        // Player to move is in check
        if (isInCheck) {
            evaluation -= CHECK_PENALTY * (isWhite ? 1 : -1);
        }
        
        // Swarm heuristic - bonus for pieces near enemy king
        // Push heuristic - bonus for advancing pieces toward enemy
        evaluation += CalculateSwarmAndPushBonus(board);

        if (!isInCheck) {
            // MinOpptMove heuristic - prefer to leave opponent with fewer possible responses\
            evaluation += VALUE_PER_AVAILABLE_MOVE * legalMoves.Length * (isWhite ? 1 : -1);

            board.MakeMove(Move.NullMove);
            // Use cache to check if null move led to a known game-over state
            var entry = getAlphaBetaCacheEntry(monotonicKey, board);
            if (entry is not { QuietDepth: byte.MaxValue, TotalDepth: byte.MaxValue }) {
                Span<Move> opponentLegalMoves = stackalloc Move[MAX_NUMBER_LEGAL_MOVES];
                board.GetLegalMovesNonAlloc(ref opponentLegalMoves);
                evaluation -= VALUE_PER_AVAILABLE_MOVE * opponentLegalMoves.Length * (isWhite ? 1 : -1);
            }
            board.UndoMove(Move.NullMove);
        }
        
        return evaluation;
    }

    // Positive favors white. Cache shared between both sides.
    public static long EvaluateMaterial(Board board) {
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
                if (pieceType == Pawn) {
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
            if (pieceType != None) {
                long pieceBonus = 0;
                Square enemyKingSquare = board.GetKingSquare(!pieceIsWhite);
                int distance = CalculateKingDistance(square, enemyKingSquare.Index);
                pieceBonus -= distance * PIECE_SWARM_VALUES[(int)pieceType];
                int rank = square >> 3;
                int file = square & 7;
                pieceBonus += (pieceIsWhite ? WHITE_RANK_ADVANCEMENT_VALUES : BLACK_RANK_ADVANCEMENT_VALUES)[rank]
                    * PIECE_RANK_PUSH_VALUES[(int)pieceType];
                if (rank != (pieceIsWhite ? 0 : 7) && pieceType != Rook) {
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
        const long ALMOST_INFINITE = 900_000_000_000L;
        if (materialEval >= BARE_KING_EVAL) {
            // A draw is almost as good as a win for a bare king since it's the best he can do
            return -ALMOST_INFINITE;
        }
        if (materialEval <= -BARE_KING_EVAL) {
            // A draw is almost as good as a win for a bare king since it's the best he can do
            return ALMOST_INFINITE;
        }

        if (materialEval < 0) {
            // Black is ahead on material, so white favors the draw
            return DRAW_VALUE_WHEN_BEHIND;
        }
        if (materialEval > 0) {
            // White is ahead on material, so black favors the draw
            return -DRAW_VALUE_WHEN_BEHIND;
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