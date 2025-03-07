using System.Runtime.InteropServices;

namespace auto_Bot_1337;

using System.Diagnostics;
using System.Linq;
using System;
using ChessChallenge.API;

public class Bot_1337 : IChessBot {
    private const byte MAX_DEPTH = 4;
    private const long INFINITY = 1_000_000_000_000;
    
    private const long MY_PIECE_VALUE_PER_CAPTURING_MOVE_MULTIPLIER = 20_000;
    private const long PENALTY_PER_ENEMY_MOVE = 1_000_000;
    private const long MIN_DRAW_VALUE_WHEN_BEHIND = 0;
    private const long MAX_DRAW_VALUE_WHEN_BEHIND = 900_000_000;
    private const long MAX_MOVE_VALUE_NOISE = 10_000;
    // private const long SAME_PIECE_OPENING_MOVE_PENALTY = 1_000_000;
    private const long CHECK_PENALTY = 1_000_000_000;
    // private const long CHECK_BONUS = 1_000_000;
    private const long CASTLING_RIGHT_VALUE = 2_000_000;
    private static readonly long[] PIECE_VALUES = [0, 100_000_000, 305_000_000, 333_000_000, 563_000_000, 950_000_000, 0];
    private static readonly long[] PIECE_RANK_PUSH_VALUES = [0, 400, 400, 800, 1200, 1600, 400];
    private static readonly long[] PIECE_FILE_PUSH_VALUES = [0, 100, 500, 400, 600, 1200, 300];
    private static readonly long[] PIECE_SWARM_VALUES = [0, 75, 150, 300, 400, 450, 200];
    private static readonly long[] WHITE_PASSED_PAWN_VALUES = [0, 0, 0, 0, 16, 64, 128, 0];
    private static readonly long[] BLACK_PASSED_PAWN_VALUES = [0, 128, 64, 16, 0, 0, 0, 0];
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
    
    private static Dictionary<ulong, long> materialEvalCache = new();
    private Dictionary<ulong, CacheEntry> alphaBetaCache = new();
    public Dictionary<ulong, long> mateOrDrawCache = new();

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
        
        var legalMoves = board.GetLegalMoves();
        
        foreach (var move in legalMoves) {
            board.MakeMove(move);
            long value = -AlphaBeta(board, MAX_DEPTH - 1, -INFINITY, INFINITY, !board.IsWhiteToMove);
            board.UndoMove(move);
            value += noise();
            
            if (bestMove.IsNull || value > bestValue || (value == bestValue && random.Next(2) != 0)) {
                bestValue = value;
                bestMove = move;
            }
        }
        
        return bestMove;
    }

    private long AlphaBeta(Board board, byte depth, long alpha, long beta, bool maximizingPlayer) {
        ulong key = board.ZobristKey;
        // Cache lookup
        if (alphaBetaCache.TryGetValue(key, out var entry) && entry.Depth >= depth) {
            if (entry.NodeType == EXACT) return entry.Score;
            if (entry.NodeType == LOWERBOUND) alpha = Math.Max(alpha, entry.Score);
            if (entry.NodeType == UPPERBOUND) beta = Math.Min(beta, entry.Score);
            if (alpha >= beta) return entry.Score;
        }
        long score;
        if (mateOrDrawCache.TryGetValue(key, out score)) {
            goto cacheStore;
        }
        if (board.IsInsufficientMaterial() || board.IsFiftyMoveDraw()) {
            score = evaluateDraw(EvaluateMaterial(board));
            mateOrDrawCache[key] = score;
            goto cacheStore;
        }
        
        var legalMoves = board.GetLegalMoves();
        if (legalMoves.Length == 0) {
            if (board.IsInCheck()) {
                // Checkmate
                score = maximizingPlayer ? -INFINITY : INFINITY;
            } else {
                // Stalemate
                score = evaluateDraw(EvaluateMaterial(board));
            }
            mateOrDrawCache[key] = score;
            goto cacheStore;
        }
        
        // Check this after GetLegalMoves, so that IsInCheck hits cache
        if (board.IsRepeatedPosition()) {
            // Don't store in mateOrDrawCache, because Board's repetition rule is 2-fold while Arbiter's is 3-fold
            long baseScore = EvaluateMaterial(board);
            if (board.IsInCheck()) {
                baseScore -= CHECK_PENALTY;
            }    
            score = evaluateDraw(baseScore);
            goto cacheStore;
        }
        if (depth == 0) {
            score = EvaluatePosition(board);
            goto cacheStore;
        }
        if (maximizingPlayer) {
            score = -INFINITY;
            foreach (var move in legalMoves) {
                board.MakeMove(move);
                long eval = AlphaBeta(board, (byte) (depth - 1), alpha, beta, false);
                board.UndoMove(move);

                score = Math.Max(score, eval);
                alpha = Math.Max(alpha, eval);
                if (beta <= alpha)
                    break;
            }
        } else {
            score = INFINITY;
            foreach (var move in legalMoves) {
                board.MakeMove(move);
                long eval = AlphaBeta(board, (byte) (depth - 1), alpha, beta, true);
                board.UndoMove(move);

                score = Math.Min(score, eval);
                beta = Math.Min(beta, eval);
                if (beta <= alpha)
                    break;
            }
        }
        // Cache store
        cacheStore:
        var nodeType = score <= alpha ? UPPERBOUND : 
            score >= beta ? LOWERBOUND : EXACT;

        alphaBetaCache[key] = new CacheEntry(
            score, depth, nodeType
        );
        return score;
    }

    private static long noise() {
        return random.NextInt64(MAX_MOVE_VALUE_NOISE) - random.NextInt64(MAX_MOVE_VALUE_NOISE);
    }

    // Positive favors the player to move
    private long EvaluatePosition(Board board) { 
        var evaluation = EvaluateMaterial(board);
        bool isWhite = board.IsWhiteToMove;
        evaluation *= (isWhite ? 1 : -1);

        // Check penalty
        if (board.IsInCheck()) {
            evaluation -= CHECK_PENALTY;
        }
        
        // Swarm heuristic - bonus for pieces near enemy king
        evaluation += CalculateSwarmBonus(board, isWhite);

        // Push heuristic - bonus for advancing pieces toward enemy
        evaluation += CalculatePushBonus(board, isWhite);

        // MinOpptMove heuristic - evaluate opponent's best response
        evaluation += minOpptMovesScore(board.GetLegalMoves());

        return evaluation;
    }

    // Positive favors white. Cache shared between both sides.
    private static long EvaluateMaterial(Board board) {
        return materialEvalCache.GetOrCreate(board.ZobristKey, () => {
            // Material and basic position evaluation
            long evaluation = 0;
            if (isBareKing(board.WhitePiecesBitboard)) {
                Debug.WriteLine("Skipping material evaluation: white is a bare king!");
                evaluation = BARE_KING_EVAL;
            }
            else if (isBareKing(board.BlackPiecesBitboard)) {
                Debug.WriteLine("Skipping material evaluation: black is a bare king!");
                evaluation = -BARE_KING_EVAL;
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
            if (board.HasKingsideCastleRight(true)) {
                evaluation += CASTLING_RIGHT_VALUE;
            }
            if (board.HasQueensideCastleRight(true)) {
                evaluation += CASTLING_RIGHT_VALUE;
            }
            if (board.HasKingsideCastleRight(false)) {
                evaluation -= CASTLING_RIGHT_VALUE;
            }
            if (board.HasQueensideCastleRight(false)) {
                evaluation -= CASTLING_RIGHT_VALUE;
            }
            return evaluation;
        });
    }

    private long CalculateSwarmBonus(Board board, bool isWhite) {
        long bonus = 0;
        Square enemyKingSquare = board.GetKingSquare(!isWhite);
        
        for (int square = 0; square < 64; square++) {
            Piece piece = board.GetPiece(SQUARES[square]);
            if (piece.IsWhite == isWhite && !piece.IsNull) {
                int distance = CalculateKingDistance(square, enemyKingSquare.Index);
                bonus += (7 - distance) * PIECE_SWARM_VALUES[(int) piece.PieceType];
            }
        }
        
        return bonus;
    }

    private long CalculatePushBonus(Board board, bool isWhite) {
        long bonus = 0;
        
        for (int square = 0; square < 64; square++) {
            Piece piece = board.GetPiece(SQUARES[square]);
            if (piece.IsWhite == isWhite && !piece.IsNull) {
                int rank = square >> 3;
                int file = square & 7;
                var pieceType = (int) piece.PieceType;
                if (isWhite) {
                    bonus += WHITE_RANK_ADVANCEMENT_VALUES[rank] 
                             * PIECE_RANK_PUSH_VALUES[pieceType];
                } else {
                    bonus += BLACK_RANK_ADVANCEMENT_VALUES[rank]
                             * PIECE_RANK_PUSH_VALUES[pieceType];
                }
                if (rank != (isWhite ? 0 : 7)) {
                    bonus -= FILE_CENTER_DISTANCE_VALUES[file]
                             * PIECE_FILE_PUSH_VALUES[pieceType];
                }
            }
        }
        
        return bonus;
    }

    private static long minOpptMovesScore(Move[] responses) {
        return -responses.Sum(m =>
            PENALTY_PER_ENEMY_MOVE
            + PIECE_VALUES[(int)m.CapturePieceType] * MY_PIECE_VALUE_PER_CAPTURING_MOVE_MULTIPLIER);
    }
    
    private static long evaluateDraw(long materialEval) {
        if (materialEval >= BARE_KING_EVAL) {
            // A draw is almost as good as a win for a bare king since it's the best he can do
            return 900_000_000_000L;
        }
        if (materialEval <= -BARE_KING_EVAL) {
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