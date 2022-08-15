{-# LANGUAGE TemplateHaskell #-}
module GameLogic  where

-- --- Chess Game ---- --------------------------------------------------------
--
-- @author: rosario <berosario@hotmail.com>
--
-- 
--  Chess Rules:
--     8 x 8 Board 
--     2     Players: White and Black. White starts and alternate moves
--     2 * 6 Pieces: King, Queen, Rook, Bishop, Knight, Pawn 
--           Each piece has its own method of movement and capturing
--           Pieces can only move to empty squares excepts when capturing
--           Pieces can not jump over other Pieces except any Knight move or Castling*  
----          
--
--     Chess Board Initial Configuration:
--
--          -----------------------
--         | ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ |              
--         | ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ |     
--         | .  .  .  . .  .  .  . |
--         | .  .  .  . .  .  .  . |
--         | .  .  .  . .  .  .  . |
--         | .  .  .  . .  .  .  . |
--         | ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙ |                              
--         | ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ |                              
--          -----------------------                      
--
--                  
--    
--     Movements:
--        King    -> 1         square in each direction. Can  
--                -> 2         square in horizontal direction. special move: Castling*
--        Rook    -> any       square in vertical or horizontal directions; special move: Castling* 
--        Bishop  -> any       square in diagonal direction
--        Queen   -> any       square in vertical, horizontal or diagonal directions 
--        Knight  -> 1,2       square in vertical + horizontal directions 
--                   2,1       square in horizontal + vertical directions
--        Pawn    -> 1         square in forward vertical direction
--                -> 1         square in any forward diagonal direction IF capturing an enemy piece; specal move: *EnPassant*
--                -> 2         square in forward vertical direction IF it is pawn first move 
--                -> _         special move: Promotion* IF reaches the end of the board
--
--
--
--    Special Movements:
--        
--        (*) Castling: King moves 2 squares in the direction of the Rook which swiches sides with the King. Only possible if fullfils all Castling Rules: 
--                              1. King and Rook first move
--                              2. No pieces between King and Rook
--                              3. King is not in Check
--                              4. King can not pass through a square attacked by the enemy
--
--              En Passsant: When a Pawn moves two squares and passes over a square attacked by enemy Pawn. If the Pawn is not immediatly captured it loses the right to EnPassant 
--                     
--              Promotion  : Once a Pawn reaches rank 8 it can be promoted to any Piece desired (defaulted to Queen)
--
--
--
--   Check: When a enemy piece is attacking the King
--
--       Check Rules:
--            0. King can never move to a square attacked
--            1. Move the King to a square not attacked
--            2. Capture the checking Piece
--            3. Block the check by moving Piece in Between
--
--   Game Over:
--      1. King is captured 
--      2. Player resigns 
--      3. Draw is declared
--
--
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
import DataTypes

import Data.Either
import Control.Lens


initGame :: String -> String -> Float -> WorldState  --change last parameter
initGame p1 p2 t = WorldState board cursor white black White msg (0,initGame p1 p2 t)
    where
      pSeq = [Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook]
      init :: Int -> Int -> Piece
      init x y
           | x == 0    = Piece (pSeq !! y) Black Init
           | x == 1    = Piece Pawn Black Init
           | x == 6    = Piece Pawn White Init
           | x == 7    = Piece (pSeq !! y) White Init
      board = [Square (x,y) (init x y) | x <- [0,1,6,7], y <- [0..7]]
      cursor = Cursor (0,0) Nothing []
      white = Player p1 t White Play
      black = Player p2 t Black Play
      msg = "game initialized"


-- helper funcs to handle board: 

-- Update PState according to move from a to b
updatePState :: Pos -> Pos -> Piece -> Piece
updatePState from to p = if (ptype p == Pawn && distance from to == 2) 
                         then p { pstate = TwoStep } 
                         else p { pstate = Moved }


-- getPiece: from position get piece
getPiece :: Pos -> Board -> Either String Piece
getPiece p b
  | np == 0     = Left "no piece in this position"
  | np == 1     = Right $ piece $ head findPiece
  | otherwise  = Left "more than one piece in the same position"
  where np = length findPiece
        findPiece = filter (\s -> pos s == p) b
--
-- setPiece: add (pos piece) to Board 
setPiece :: Piece -> Pos -> Board -> Board
setPiece piece pos b = b ++ [Square pos piece]

--
--deletePiece: delete piece 
deletePiece :: Pos -> Board -> Board
deletePiece p = filter (\s -> not (pos s == p))
                         
--
-- move piece: delete piece from curr position and add piece to new position
-- update piece state before setting piece
movePiece :: Pos -> Pos -> Board -> Board
movePiece from to b =
  case getPiece from b of
    Right p0  -> setPiece p to $ deletePiece from $ deletePiece to b
                   where p = updatePState from to p1  -- update PState
                         p1 = if isPromotion from to b then 
                               promotePiece p0 Queen  -- promote to Queen by default
                             else
                               p0
    Left m    -> error "should never try to move piece that doesnt exist"


 
-- promotePiece: change piece type 
promotePiece :: Piece -> PType -> Piece
promotePiece (Piece pt c s) pT = Piece pT c s


-- Promoted if Pawn moves to the last line
isPromotion :: Pos -> Pos -> Board -> Bool
isPromotion from to b =  (ptype piece == Pawn)          -- it's a pawn 
                     &&  (fst to == 0 || fst to == 7)   -- reaches last line 
   where piece = case getPiece from b of
                    Left m  -> error $ "Tested isPromotion with "++m
                    Right p -> p 


-- get the other Pcolor
other :: PColor -> PColor
other White =  Black
other Black =  White

isEmpty :: [a] -> Bool
isEmpty = \myList ->
  case myList of
    [] -> True -- if the list is empty, return true
    _ ->  False -- otherwise, return false


ws = initGame "me" "you" 3
b = ws ^. board
b1 =  movePiece (0,3) (6,3) b
b2 =  movePiece (7,3) (1,3) b1
b3 =  movePiece (1,3) (1,2) b2
b4 =  movePiece (7,2) (4,3) b3
b5 =  movePiece (7,1) (5,0) b4
b6 =  movePiece (6,3) (5,4) b5
b7 =  movePiece (5,4) (5,0) b6



--------------------------------------------------------------
isCastling :: Piece -> Pos -> Pos -> Board -> Bool
isCastling p from to b = ptype p == King             -- piece King 
                     && pstate p == Init             -- state Init
                     && d == 2 && fst from == fst to -- 2 squares same row
                     && isRookAvail                  -- Rook is available
                     && isPathClear p from to b      -- king has clear path
                     && not(isPathAttacked color from to b)-- king path NOT attacked
                     && isPathClear p rFrom rTo b    -- rook has clear path
  where color = pcolor p
        d = distance from to
        (rFrom,rTo) = castlingRookMove (from,to)    -- get rook move (from,to)
        rooklist    = findPieces Rook color b       -- find Rooks  
        rooksInit   = filter (\s -> pstate (piece  s) == Init) rooklist 
        rook        = filter (\s -> (pos s)          == rFrom) rooksInit
        isRookAvail = not $ isEmpty rook
                            
    
         
-- Get rook move from king move in castling 
castlingRookMove :: (Pos, Pos) -> (Pos, Pos) 
castlingRookMove (from, to) = let row = fst from
                              in if (snd from < snd to) then ((row, 7), (row, 5)) 
                                                        else ((row, 0), (row, 3))

findPieces :: PType -> PColor -> Board -> Board
findPieces pt c b = filter (\s -> ptype (piece s) == pt) $
                    filter (\s -> pcolor (piece s) == c) b

-- check if the path (from->to) cross any attacked square
isPathAttacked :: PColor -> Pos -> Pos -> Board -> Bool
isPathAttacked c from to b = any (\b -> b == True) pathAtt
    where  path = getPath from to ++ [to]        -- include place where king lands
           pathAtt = map (\pos -> isAttacked c pos b) path
---------------------------------------------------------


isCheck :: PColor -> Board -> Bool
isCheck color board = isAttacked color kingPos board
  where kingPos = getKingPos color board

-- from Player color and position 
-- check if that position is attacked
isAttacked :: PColor -> Pos -> Board -> Bool
isAttacked player pos b = 
    let allMvTup     = allPossibleMoves (other player) b
        allMvLst     = concat $ map (\(_,a)->a) allMvTup
    in  elem pos allMvLst

-- getPosistion: from piece get position
getKingPos :: PColor -> Board -> Pos
getKingPos col b  = pos king where
   king = head $ filter (\s -> (ptype  (piece s) == King)  
                     && (pcolor (piece s) == col)) b    
-------------------------------------------------------

                   
-- reset pawns in two step state
resetTwoStep :: PColor -> Board -> Board
resetTwoStep color board =  map f board 
  where f (Square pos (Piece pt pc TwoStep)) = Square pos (Piece pt pc Moved)        
        f sq = sq                                  
                        
-------------------------------------------
--EnPassant       

 
        
        
-------------------------------------------

         

-- from Color get a list of all pieces of that color that are on the board  
-- from list of pieces on the board
-- create tupple with (Piece, [piece possible moves])
allPossibleMoves :: PColor -> Board -> [(Piece,[Pos])]
allPossibleMoves color b = 
    let allP        = filter (\sq -> pcolor (piece sq) == color ) b
        moveLst sq  = getPossibleMoves (piece sq) (pos sq) b
    in  map (\z -> (piece z, moveLst z)) allP
     

-- possibleMoves: from Piece and Position get all possible moves for that piece on the Board
getPossibleMoves :: Piece -> Pos -> Board -> [Pos]  -- missing special moves (Castling and Enpassant)
getPossibleMoves p from b = 
    let allpos = [(x,y) | x <-[0..7], y <- [0..7]] 
    in filter (\to -> if isSpaceFree to b then 
                         isValidMove p from to 
                      && isPathClear p from to b
                      else isCapture p to b  
                      &&   isValidStep p from to
                      &&   isPathClear p from to b) allpos  

-- true if it lands in an Enemy Piece  
isCapture :: Piece -> Pos -> Board -> Bool
isCapture p to b = case getPiece to b of
              Left m    -> False
              Right p2  -> if (pcolor p == pcolor p2) 
                            then False 
                            else True

-- true if it lands in empty space
isSpaceFree :: Pos -> Board -> Bool
isSpaceFree space board = case getPiece space board of
              Left  m   -> True
              Right p   -> False

-- True if the path between two positions is clear: except Knight
isPathClear :: Piece -> Pos -> Pos -> Board -> Bool
isPathClear p from to b       
  | (ptype p == Knight) = True
  | (d > 1)             = all (\p -> p == False) pieces
  | otherwise           = True 
  where d = distance from to
        path   = getPath from to
        pieces = map (\pth -> case getPiece pth b of Right p -> True
                                                     Left m  -> False) path

getPath :: Pos -> Pos -> [Pos]
getPath (x0,y0) (x1,y1) = [ ((x0+i*s), (y0+j*s)) | s <- [1..d-1]] 
  where d = distance (x0,y0) (x1,y1)
        (i,j) = (div (x1-x0) d, div (y1-y0) d)  -- (-1 or 0 or 1)      
       
       

-- distance between two positions
distance :: Pos -> Pos -> Int
distance (a0,b0) (a1,b1) = max (abs(a1-a0)) (abs(b1-b0))

-- isValidStep: adds option of Pawns capture to validMove
isValidStep :: Piece -> Pos -> Pos -> Bool
isValidStep (Piece Pawn Black _) (x0,y0) (x1,y1) = (x1-x0) == 1 && abs (y1-y0) == 1
isValidStep (Piece Pawn White _) (x0,y0) (x1,y1) = (x0-x1) == 1 && abs (y1-y0) == 1
isValidStep  p                   from    to      = isValidMove p from to


-- isValidMove' -> is this piece capable of moving 'from' 'to'
-- this functions ignores other pieces on the board (must add exception to pawn capture)
isValidMove :: Piece -> Pos -> Pos -> Bool
isValidMove (Piece King _ _)    from to = distance from to <= 1  
isValidMove (Piece Rook _ _)    from to = isStraight from to
isValidMove (Piece Bishop _ _)  from to = isDiagonal from to
isValidMove (Piece Queen _ _)   from to = isStraight from to || 
                                          isDiagonal from to
isValidMove (Piece Knight _ _ ) from to = isL from to
isValidMove (Piece Pawn White _ ) from to = isWForward from to  
isValidMove (Piece Pawn Black _ ) from to = isBForward from to 

-- valid move auxliary functions
isStraight :: Pos -> Pos -> Bool
isStraight (x0,y0) (x1,y1) = x1-x0 == 0 || y1-y0 == 0
isDiagonal :: Pos -> Pos -> Bool
isDiagonal (x0,y0) (x1,y1) = abs(x1-x0) == abs(y1-y0)
isL :: Pos -> Pos -> Bool
isL (x0,y0) (x1,y1) = (abs(x1-x0),abs(y1-y0)) == (1,2) ||
                      (abs(x1-x0),abs(y1-y0)) == (2,1)
isVertical :: Pos -> Pos -> Bool
isVertical (x0,y0) (x1,y1) = (y1-y0) == 0
isWForward :: Pos -> Pos -> Bool
isWForward (x0,y0) (x1,y1)  = isVertical (x0,y0) (x1,y1) &&
                          (x0-x1 == 1 ||(x0 == 6 && x0-x1 == 2)) 
isBForward  :: Pos -> Pos -> Bool
isBForward (x0,y0) (x1,y1)  = isVertical (x0,y0) (x1,y1) &&
                          (x1-x0 == 1 ||(x0 == 1 && x1-x0 == 2))
----------------------------------------
