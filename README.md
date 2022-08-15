# haskell-chess-terminal

Simple chess game written in Haskell

Cabal directory contains all code and libraries necessary to compile and run it on your terminal



------  Chess Game Logic-----------------------------------------------------------

 
  Chess Rules:
    8 x 8 Board 
    2     Players: White and Black. White starts and alternate moves
    2 * 6 Pieces: King, Queen, Rook, Bishop, Knight, Pawn 
           Each piece has its own method of movement and capturing
          Pieces can only move to empty squares excepts when capturing
          Pieces can not jump over other Pieces except any Knight move or Castling*  
          

     Chess Board Initial Configuration:

          ------------------
         | ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ |              
         | ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ |     
         | . . . . . . . . |
         | . . . . . . . . |
         | . . . . . . . . |
         | . . . . . . . . |
         | ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙ |                              
         | ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ |                              
          ------------------                     
   
     Movements:
        King    -> 1         square in each direction. Can  
                -> 2         square in horizontal direction. special move: Castling*
        Rook    -> any       square in vertical or horizontal directions; special move: Castling* 
        Bishop  -> any       square in diagonal direction
        Queen   -> any       square in vertical, horizontal or diagonal directions 
        Knight  -> 1,2       square in vertical + horizontal directions 
                   2,1       square in horizontal + vertical directions
        Pawn    -> 1         square in forward vertical direction
                -> 1         square in any forward diagonal direction IF capturing an enemy piece; specal move: *EnPassant*
                -> 2         square in forward vertical direction IF it is pawn first move 
                -> _         special move: Promotion* IF reaches the end of the board
 
 Special Movements:
        
        (*) Castling: King moves 2 squares in the direction of the Rook which swiches sides with the King. Only possible if fullfils all Castling Rules: 
                              1. King and Rook first move
                              2. No pieces between King and Rook
                              3. King is not in Check
                              4. King can not pass through a square attacked by the enemy

              En Passsant: When a Pawn moves two squares and passes over a square attacked by enemy Pawn. If the Pawn is not immediatly captured it loses the right to EnPassant 
                     
              Promotion  : Once a Pawn reaches rank 8 it can be promoted to any Piece desired (defaulted to Queen)



 Check: When a enemy piece is attacking the King

     Check Rules:
          0. King can never move to a square attacked
          1. Move the King to a square not attacked
          2. Capture the checking Piece
          3. Block the check by moving Piece in Between

 Game Over:
    1. King is captured 
    2. Player resigns 
    3. Draw is declared




