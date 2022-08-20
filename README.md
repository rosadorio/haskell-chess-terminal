# haskell-chess-terminal-game

Project developed in the context of *Emurgo's Cardano Developer Associate Programme*. This is my final project of the *Haskell Programming* module that aims at demonstrating haskell basic profeciency

# Intro

This is a simple chess terminal game that contains all basic chess rules including special moves: *Castling, Enpassant and pawn Promotion*. 
(Pawns are promoted to Queen by default)

Once the game is running the board is displayed on the terminal and you can use "wasd" to move the cursor to select pieces and movement

![Process](demo.png)

## Game Features

Short list of features included on current release:
1. When piece selected it highlights its possible moves
2. When in check the King is highlighted
3. Check-mate is declared and the game terminates when there is no possible move to take the king out of check 
4. Option to resign
5. Option to rewind to previous turn 


## Future Evolution
1. Adding option to propose draw
2. Add time limit per turn 

# Getting Started 
Repo contains all the necessary sources to build and run it on your terminal. To build it, it is necessary to have cabal installed in your system

Instruction to build and run game:

 `git clone git@github.com:rosadorio/haskell-chess-terminal.git`

 `cd haskell-chess-terminar`

 `cabal update && cabal install --only-dependencies`
 
 `cabal build`
 
 `cabal run`




