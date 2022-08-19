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


-- initialize game
initGame :: String -> String -> Float -> WorldState  --change last parameter
initGame p1 p2 t = WorldState board cursor white black White msg [(0,board)]
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




-- move piece: delete piece from curr position and add piece to new position
-- update piece state before setting piece
movePiece :: Pos -> Pos -> Board -> Board
movePiece from to b
  | isEnpassant p from to b = doEnpassant
  | isCastling  p from to b = doCastling
  | isPromotion from to b   = doPromotion
  | otherwise               = move pUp from to b
  where  doEnpassant = let (r0,c0) = from; (r1,c1) = to 
                       in  move pUp from to $ deletePiece (r0,c1) b 
         doCastling  = let (rfrom,rto) = castlingRookMove (from,to)
                           rook = case getPiece rfrom b of
                                    Right rk  -> updatePState rfrom rto rk
                                    Left  m   -> p -- should never happen if isCastling
                       in move pUp from to $ move rook rfrom rto b
         doPromotion = let pp = promotePiece pUp Queen in move pp from to b              
         pUp  = updatePState from to p 
         p    = case getPiece from b of
                  Right p  -> p
                  Left m   -> error "should never try to move piece that doesnt exist"
         move p from to b  = setPiece p to $ deletePiece from $ deletePiece to b




---- helper funcs to handle board ----

-- getPiece: from position get piece
getPiece :: Pos -> Board -> Either String Piece
getPiece p b
  | np == 0     = Left "no piece in this position"
  | np == 1     = Right $ _piece $ head findPiece
  | otherwise   = Left "more than one piece in the same position"
  where np = length findPiece
        findPiece = filter (\s -> s^.pos == p) b
--
-- setPiece: add (pos piece) to Board 
setPiece :: Piece -> Pos -> Board -> Board
setPiece piece pos b = b ++ [Square pos piece]

--
--deletePiece: delete piece 
deletePiece :: Pos -> Board -> Board
deletePiece p = filter (\s -> not (s^.pos == p))
                         

-- promotePiece: change piece type 
promotePiece :: Piece -> PType -> Piece
promotePiece (Piece pt c s) newPt = Piece newPt c s


-- Update PState according to move from a to b
updatePState :: Pos -> Pos -> Piece -> Piece
updatePState from to p = if (p^.ptype == Pawn && distance from to == 2) 
                         then p { _pstate = TwoStep } 
                         else p { _pstate = Moved }

-- get the other Pcolor
other :: PColor -> PColor
other White =  Black
other Black =  White

isEmpty :: [a] -> Bool
isEmpty = \myList ->
  case myList of
    [] -> True -- if the list is empty, return true
    _ ->  False -- otherwise, return false


                   
-- set pawns in two step state
setTwoStepMoved :: PColor -> Board -> Board
setTwoStepMoved color board =  map f board 
  where f sq = if sq^.piece^.pcolor == color && sq^.piece^.pstate == TwoStep then  
                    Square (sq^.pos) (Piece Pawn color Moved)        
               else sq                    



------------------------------------------
--EnPassant       
isEnpassant :: Piece -> Pos -> Pos -> Board -> Bool
isEnpassant p (r0,c0) (r1,c1) b = isPawn p && isValidMv && isEnPawnInRow    
   where 
      isEnPawnInRow = case getPiece (r0,c1) b of
                        Left  m -> False
                        Right p1 ->isPawn p1 && isTwoStep p1 && isEnemy p1                
      isPawn    pi    = pi ^. ptype  == Pawn
      isTwoStep p1    = p1 ^. pstate == TwoStep
      isEnemy   p1    = p1 ^. pcolor == (other $ p ^. pcolor )
      isValidMv       = isValidStep p (r0,c0) (r1,c1) 
-- didnt check if the space is free but assuming TwoStep is working it should never happen


--------------------------------------------------------------
isCastling :: Piece -> Pos -> Pos -> Board -> Bool
isCastling p from to b = _ptype p == King             -- piece King 
                      && _pstate p == Init             -- state Init
                      && d == 2 && fst from == fst to -- 2 squares same row
                      && isRookAvail                  -- Rook is available
                      && isPathClear p from to b      -- king has clear path
                      && not(isPathAttacked color from to b)-- king path NOT attacked
                      && isPathClear p rFrom rTo b    -- rook has clear path
  where color = _pcolor p
        d = distance from to
        (rFrom,rTo) = castlingRookMove (from,to)    -- get rook move (from,to)
        rooklist    = findPieces Rook color b       -- find Rooks  
        rooksInit   = filter (\s -> s^.piece^.pstate == Init) rooklist 
        rook        = filter (\s -> s^.pos           == rFrom) rooksInit
        isRookAvail = not $ isEmpty rook
                            
    
         
-- Get rook move from king move in castling 
castlingRookMove :: (Pos, Pos) -> (Pos, Pos) 
castlingRookMove (from, to) = let row = fst from
                              in if (snd from < snd to) then ((row, 7), (row, 5)) 
                                                        else ((row, 0), (row, 3))

findPieces :: PType -> PColor -> Board -> Board
findPieces pt c b = filter (\sq -> sq^.piece^.ptype == pt) $
                    filter (\sq -> sq^.piece^.pcolor == c) b

-- check if the path (from->to) cross any attacked square
isPathAttacked :: PColor -> Pos -> Pos -> Board -> Bool
isPathAttacked c from to b = any (\b -> b == True) pathAtt
    where  path = getPath from to ++ [to]        -- include place where king lands
           pathAtt = map (\pos -> isAttacked c pos b) path
           
---------------------------------------------------------

-- Promoted if Pawn moves to the last line
isPromotion :: Pos -> Pos -> Board -> Bool
isPromotion from to b =  (piece^.ptype == Pawn)          -- it's a pawn 
                     &&  (fst to == 0 || fst to == 7)   -- reaches last line 
   where piece = case getPiece from b of
                    Left m  -> error $ "isPromotion failed "++m
                    Right p -> p 
                    

----------------------------------------------------------


-- possibleMoves: from Piece and Position get all possible moves for that piece on the Board
getPossibleMoves :: Piece -> Pos -> Board -> [Pos] 
getPossibleMoves p from b = 
    let allpos = [(x,y) | x <-[0..7], y <- [0..7]] 
    in filter (\to -> if isSpaceFree to b then 
                          isEnpassant p from to b
                      || isCastling   p from to b
                      || isValidMove p from to 
                      && isPathClear p from to b
                      else isCapture p to b  
                      &&   isValidStep p from to
                      &&   isPathClear p from to b) allpos  

-- true if it lands in an Enemy Piece  
isCapture :: Piece -> Pos -> Board -> Bool
isCapture p to b = case getPiece to b of
              Left m    -> False
              Right p2  -> if (p^.pcolor ==  p2^.pcolor ) 
                            then False 
                            else True

-- true if it lands in empty space
isSpaceFree :: Pos -> Board -> Bool
isSpaceFree space board = case getPiece space board of
              Left  m   -> True
              Right p   -> False

-- True if the path between two positions is clear: except Knight always true
isPathClear :: Piece -> Pos -> Pos -> Board -> Bool
isPathClear p from to b       
  | (p^.ptype == Knight) = True
  | (d > 1)              = all (\p -> p == True) freeSpc
  | otherwise            = True 
  where d       = distance from to
        path    = getPath from to
        freeSpc = map (\pos -> isSpaceFree pos b) path

getPath :: Pos -> Pos -> [Pos]
getPath (x0,y0) (x1,y1) = [ ((x0+i*s), (y0+j*s)) | s <- [1..d-1]] 
  where d = distance (x0,y0) (x1,y1)
        (i,j) = (div (x1-x0) d, div (y1-y0) d)  -- (-1 or 0 or 1)      
       

-----------------------------------------------------------

isCheck :: PColor -> Board -> Bool
isCheck player board = isAttacked player kingPos board
  where kingPos = getKingPos player board

-- from Player color and position 
-- check if that position is attacked
isAttacked :: PColor -> Pos -> Board -> Bool
isAttacked player pos b = 
    let allMvTup     = allPossibleMoves (other player) b
        allMvLst     = concat $ map (\(_,_,a) -> a ) allMvTup
    in  elem pos allMvLst

-- getPosistion: from piece get position
getKingPos :: PColor -> Board -> Pos
getKingPos col b  = _pos king where
   king = head $ filter (\sq -> (sq^.piece^.ptype == King)  
                     && (sq^.piece^.pcolor == col)) b   

----------------------------------------------------------



-- input: color, board -> return: isCheckMate Bool 
-- Get all possible moves 
-- Get all possible board outcomes
-- if in all resulting boards the player is incheck then CheckMate = True else False  
isCheckMate :: PColor -> Board -> Bool
isCheckMate player board  = all (\b -> b==True) allPossBrdInCheck  
  where allPossBrdInCheck = map (\b -> isCheck player b) allPossBoard 
        allPossBoard      = map (\(p,from,to) -> movePiece from to board) allPossMoves
        allPossMoves      = concat $ map makeMovelist allMovesTupl
        allMovesTupl      = allPossibleMoves player board
        
makeMovelist :: (Piece,Pos,[Pos]) -> [(Piece,Pos,Pos)]
makeMovelist ( _, _,    [])      = []
makeMovelist (p,from,(to:txs)) = (p,from,to):makeMovelist (p,from,txs) 
 

-- from Color get list of all pieces of that color that are on the board  
-- from list of pieces on the board
-- create tupple with (Piece, [piece possible moves])
allPossibleMoves :: PColor -> Board -> [(Piece,Pos,[Pos])]
allPossibleMoves player b = 
    let allPSqs      = filter (\sq -> sq^.piece^.pcolor == player) b -- list squares with player piece
        moveLst sq  = getPossibleMoves (sq^.piece) (sq^.pos) b      -- getPossMoves for each 
    in  map (\sq -> (sq^.piece, sq^.pos ,moveLst sq)) allPSqs       -- make list of moves
      
 
-------------------------------------------------------------------     

-- distance between two positions
distance :: Pos -> Pos -> Int
distance (a0,b0) (a1,b1) = max (abs(a1-a0)) (abs(b1-b0))

-- isValidStep: adds Pawns capture to validMove
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
--------------------------------------------

--DEBUG

ws = initGame "me" "you" 3
b = ws ^. board
b1 =  movePiece (0,3) (6,3) b
b2 =  movePiece (7,3) (1,3) b1
b3 =  movePiece (1,3) (1,2) b2
b4 =  movePiece (7,2) (4,3) b3
b5 =  movePiece (7,1) (5,0) b4
b6 =  movePiece (6,3) (5,4) b5
b7 =  movePiece (5,4) (5,0) b6
b8 =  movePiece (5,4) (5,0) b7


initB = [Square {_pos = (0,4), _piece = Piece {_ptype = King, _pcolor = Black, _pstate = Init}},Square {_pos = (7,4), _piece = Piece {_ptype = King, _pcolor = White, _pstate = Init}},Square {_pos = (0,1), _piece = Piece {_ptype = Queen, _pcolor = White, _pstate = Init}},Square {_pos = (1,1), _piece = Piece {_ptype = Queen, _pcolor = White, _pstate = Init}}]




