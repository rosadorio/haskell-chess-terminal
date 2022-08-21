# Intro haskell-chess-terminal-game

Project developed in the context of *Emurgo's Cardano Developer Associate Programme*. This is my final project of the *Haskell Programming* module that aims at demonstrating haskell basic profeciency


Bellow you can find a video with the official presentation of the project. It became a bit more extensive than I intended (sorry for that)

[![Haskell Chess Presentation Video](https://img.youtube.com/vi/sClJghPKrms/0.jpg)](https://www.youtube.com/watch?v=sClJghPKrms)


## Chess Game Rules

- Board with 8 x 8 dimensions 
- Two Players: White and Black. White always starts and alternate turns until game ends
- Each Player starts with 16 pieces
- There are 6 Pieces Types: King, Queen, Rook, Bishop, Knight, Pawn. Each piece has its own method of movement and capturing
- Pieces can only move to empty squares excepts when capturing enemy pieces
- Pieces can not jump over other Pieces except any Knight's move or in Castling  

**Movements**

- King (♔ ♚)   can move one square in each direction or two squares in horizontal direction when Castling*
- Rook (♖ ♜)  can move any number of squares in single vertical or horizontal directions; special move: Castling*
- Bishop (♗ ♝) can move any number of squares in single diagonal direction
- Queen (♕ ♛) can move any number of squares in single vertical, horizontal or diagonal directions 
- Knight (♘ ♞) can move in 'L', two horizontal one vertical square or its inverse 
- Pawn (♙ ♟) can move one square in forward vertical direction or two if it is first move. Can move one square in forward diagonal only if it is capturing enemy piece (or during special move En Passant*). If it reaches the edge of the board can be promoted to any other piece 

**Special Movements** 

- **Castling** - King moves 2 squares in the direction of the Rook which swiches sides with the King. 

>**Castling Rules that must be all fulfilled:**
>
>1. King and Rook first move
>2. Path is clear between King and Rook
>3. King is not in Check
>4. King does not pass through a square attacked by enemy


- **En Passsant** - When a Pawn 'A' moves two squares and passes over a square attacked by enemy Pawn 'B'. Pawn 'A' can be captured in following turn by Pawn 'B' as if Pawn 'A' has moved only one square. If the Pawn is not immediatly captured it loses the right to en Passant 
                     
- **Promotion** - Once a Pawn reaches rank 8 it can be promoted to any Piece desired 



**Check** When a enemy piece is attacking the King the player is forced to make a move end to end condition

>  **Check Rules:**
>
>0. King can never move to a square attacked
>1. Move the King to a square not attacked
>2. Capture the checking Piece
>3. Block the check by moving Piece in between

**Game Ends When**

1. A King is checkmated, i.e. there is no possible move that takes the King out of check 

2. A Player resigns 

3. Draw is declared (not implemented)

>  **Draw Rules:**
>
> 1. Stalemate: If the player to move has no legal move, but is not in check, the position is a stalemate, and the game is drawn.
> 2. Dead position: If neither player is able to checkmate the other by any legal sequence of moves, the game is drawn.
> 3. Draw by agreement: Reached by mutual agreement between the players. 


4. Win on Time, player runs out of time (not implemented)

# Implementation

This is a basic terminal chess game that contains all basic chess rules including special moves: *Castling, Enpassant and pawn Promotion*. 
(Pawns are promoted to Queen by default)

Once the game is running the board is displayed on the terminal and you can use "wasd" to move the cursor to select pieces and movement

![Process](demo.png)

**Game Features Included in Current Release**
1. When piece selected it highlights its possible moves
2. When in check the King is highlighted
3. Check-mate is declared and the game terminates when there is no possible move to take the king out of check 
4. Option to resign
5. Option to rewind to previous turn 


**Future Evolution**
1. Adding option to propose draw
2. Add time limit per turn 



# Getting Started 
Repo contains all the necessary sources to build and run it on your terminal. To build it, it is only necessary to have cabal installed in your system. Bellow you can see the instruction to build and run game:

 ```consol
 git clone git@github.com:rosadorio/haskell-chess-terminal.git
 cd haskell-chess-terminar
 cabal update 
 cabal install --only-dependencies
 cabal build
 cabal run
 ```





