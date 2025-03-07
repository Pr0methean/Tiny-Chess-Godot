namespace auto_Bot_1337;

using System.Diagnostics;
using System.Linq;
using System;
using ChessChallenge.API;

public class Bot_1337 : IChessBot {
    private const int MAX_DEPTH = 4;
    private const long INFINITY = 1_000_000_000_000;
    
    private const long MY_PIECE_VALUE_PER_CAPTURING_MOVE_MULTIPLIER = 20_000;
    private const long PENALTY_PER_ENEMY_MOVE = 1_000_000;
    private const long MIN_DRAW_VALUE_WHEN_BEHIND = 0;
    private const long MAX_DRAW_VALUE_WHEN_BEHIND = 900_000_000;
    private const long MAX_MOVE_VALUE_NOISE = 1_000;
    // private const long SAME_PIECE_OPENING_MOVE_PENALTY = 1_000_000;
    private const long ENEMY_CHECK_BONUS = 1_000_000_000;
    // private const long CHECK_BONUS = 1_000_000;
    // private const long CASTLING_BONUS = 2_000_000;
    private static readonly long[] PIECE_VALUES = [0, 100_000_000, 305_000_000, 333_000_000, 563_000_000, 950_000_000, 20_000_000_000];
    private static readonly long[] PIECE_RANK_PUSH_VALUES = [0, 400, 400, 800, 1200, 1600, 400];
    private static readonly long[] PIECE_FILE_PUSH_VALUES = [0, 100, 500, 400, 600, 1200, 300];
    private static readonly long[] PIECE_SWARM_VALUES = [0, 75, 150, 300, 400, 450, 200];
    private static readonly long[] WHITE_PASSED_PAWN_VALUES = [0, 0, 0, 0, 16, 64, 128, 0];
    private static readonly long[] BLACK_PASSED_PAWN_VALUES = [0, 128, 64, 16, 0, 0, 0, 0];
    private static readonly long[] FILE_CENTER_DISTANCE_VALUES = [6, 3, 1, 0, 0, 1, 3, 6];
    private static readonly long[] WHITE_RANK_ADVANCEMENT_VALUES = [0, 3, 6, 9, 11, 13, 14, 15];
    private static readonly long[] BLACK_RANK_ADVANCEMENT_VALUES = [15, 14, 13, 11, 9, 6, 3, 0];
    private static Random random = new();

    private static Dictionary<ulong, long> basicEvalCache = new();
    // Option 2: Using LINQ (more concise but perhaps less readable)
    private Dictionary<ulong, long>[] alphaBetaCache = Enumerable.Range(0, MAX_DEPTH)
        .Select(_ => new Dictionary<ulong, long>())
        .ToArray();

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
        long bestValue = -INFINITY;
        
        var legalMoves = board.GetLegalMoves();
        
        foreach (var move in legalMoves) {
            board.MakeMove(move);
            long value = -AlphaBeta(board, MAX_DEPTH - 1, -INFINITY, INFINITY, !board.IsWhiteToMove);
            board.UndoMove(move);
            
            if (value > bestValue) {
                bestValue = value;
                bestMove = move;
            }
        }
        
        return bestMove;
    }

    private long AlphaBeta(Board board, int depth, long alpha, long beta, bool maximizingPlayer) {
        long alphaBeta;
        for (int cacheDepth = MAX_DEPTH - 1; cacheDepth > depth; depth--) {
            if (alphaBetaCache[cacheDepth].TryGetValue(board.ZobristKey, out alphaBeta)) {
                return alphaBeta;
            }
        }

        return alphaBetaCache[depth].GetOrCreate(board.ZobristKey, () => {
            var legalMoves = board.GetLegalMoves();
            if (legalMoves.Length == 0) {
                if (board.IsInCheck()) {
                    // Checkmate
                    return maximizingPlayer ? -INFINITY : INFINITY;
                }
                // Stalemate
                return evaluateDraw(EvaluateBasicPosition(board));
            }

            if (board.IsRepeatedPosition() || board.IsInsufficientMaterial()) {
                return evaluateDraw(EvaluateBasicPosition(board));
            }

            if (depth == 0) {
                return EvaluatePosition(board);
            }

            if (maximizingPlayer) {
                long maxEval = -INFINITY;
                foreach (var move in legalMoves) {
                    board.MakeMove(move);
                    long eval = AlphaBeta(board, depth - 1, alpha, beta, false);
                    eval += random.NextInt64(MAX_MOVE_VALUE_NOISE) - random.NextInt64(MAX_MOVE_VALUE_NOISE);
                    board.UndoMove(move);

                    maxEval = Math.Max(maxEval, eval);
                    alpha = Math.Max(alpha, eval);
                    if (beta <= alpha)
                        break;
                }

                return maxEval;
            }

            long minEval = INFINITY;
            foreach (var move in legalMoves) {
                board.MakeMove(move);
                long eval = AlphaBeta(board, depth - 1, alpha, beta, true);
                eval += random.NextInt64(MAX_MOVE_VALUE_NOISE) - random.NextInt64(MAX_MOVE_VALUE_NOISE);
                board.UndoMove(move);

                minEval = Math.Min(minEval, eval);
                beta = Math.Min(beta, eval);
                if (beta <= alpha)
                    break;
            }

            return minEval;
        });
    }
    
    private long EvaluatePosition(Board board) { 
        var evaluation = EvaluateBasicPosition(board);
        bool isWhite = board.IsWhiteToMove;
        evaluation *= (isWhite ? 1 : -1);

        // Swarm heuristic - bonus for pieces near enemy king
        evaluation += CalculateSwarmBonus(board, isWhite);

        // Push heuristic - bonus for advancing pieces toward enemy
        evaluation += CalculatePushBonus(board, isWhite);

        // MinOpptMove heuristic - evaluate opponent's best response
        evaluation += minOpptMovesScore(board.GetLegalMoves());

        return evaluation;
    }

    private static long EvaluateBasicPosition(Board board) {
        return basicEvalCache.GetOrCreate(board.ZobristKey, () => {
            // Material and basic position evaluation
            long evaluation = 0;
            if (isBareKing(board.WhitePiecesBitboard)) {
                Debug.WriteLine("Skipping material evaluation: white is a bare king!");
                evaluation = 100_000_000_000L;
            }
            else if (isBareKing(board.BlackPiecesBitboard)) {
                Debug.WriteLine("Skipping material evaluation: black is a bare king!");
                evaluation = -100_000_000_000L;
            }
            else {
                for (int square = 0; square < 64; square++) {
                    Piece piece = board.GetPiece(new Square(square));
                    long pieceValue = PIECE_VALUES[(int)piece.PieceType]; 
                    if (piece.IsPawn) {
                        int rank = square / 8;
                        pieceValue += (piece.IsWhite ? WHITE_PASSED_PAWN_VALUES : BLACK_PASSED_PAWN_VALUES)
                            [rank];
                    }

                    evaluation += pieceValue * (piece.IsWhite ? 1 : -1);
                }

                Debug.WriteLine("Material eval: {0}", evaluation);
            }

            // Check bonus
            if (board.IsInCheck()) {
                evaluation += ENEMY_CHECK_BONUS;
            }

            return evaluation;
        });
    }

    private long CalculateSwarmBonus(Board board, bool isWhite) {
        long bonus = 0;
        Square enemyKingSquare = board.GetKingSquare(!isWhite);
        
        for (int square = 0; square < 64; square++) {
            Piece piece = board.GetPiece(new Square(square));
            if (piece.IsWhite == isWhite && !piece.IsNull) {
                int distance = CalculateManhattanDistance(square, enemyKingSquare.Index);
                bonus += (7 - distance) * PIECE_SWARM_VALUES[(int) piece.PieceType];
            }
        }
        
        return bonus;
    }

    private long CalculatePushBonus(Board board, bool isWhite) {
        long bonus = 0;
        
        for (int square = 0; square < 64; square++) {
            Piece piece = board.GetPiece(new Square(square));
            if (piece.IsWhite == isWhite && !piece.IsNull) {
                int rank = square / 8;
                int file = square % 8;
                if (isWhite) {
                    bonus += WHITE_RANK_ADVANCEMENT_VALUES[rank] 
                             * PIECE_RANK_PUSH_VALUES[(int) piece.PieceType];
                } else {
                    bonus += BLACK_RANK_ADVANCEMENT_VALUES[rank]
                             * PIECE_RANK_PUSH_VALUES[(int) piece.PieceType];
                }
                bonus -= FILE_CENTER_DISTANCE_VALUES[file]
                    * PIECE_FILE_PUSH_VALUES[(int) piece.PieceType];
            }
        }
        
        return bonus;
    }

    private static long minOpptMovesScore(Move[] responses) {
        return -responses.Sum(m =>
            PENALTY_PER_ENEMY_MOVE
            + PIECE_VALUES[(int)m.CapturePieceType] * MY_PIECE_VALUE_PER_CAPTURING_MOVE_MULTIPLIER);
    }
    
    private long EvaluateOpponentBestMove(Board board) {
        long bestEval = -INFINITY;
        var opponentMoves = board.GetLegalMoves();
        if (opponentMoves.Length == 0) {
            return 0;
        }
        foreach (var move in opponentMoves) {
            board.MakeMove(move);
            long eval = EvaluateBasicPosition(board);
            board.UndoMove(move);
            
            bestEval = Math.Max(bestEval, eval);
        }
        
        return bestEval;
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

    private int CalculateManhattanDistance(int square1, int square2) {
        int x1 = square1 % 8;
        int y1 = square1 / 8;
        int x2 = square2 % 8;
        int y2 = square2 / 8;
        
        return Math.Abs(x1 - x2) + Math.Abs(y1 - y2);
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